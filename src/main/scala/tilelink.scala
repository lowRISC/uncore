// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import scala.math.max
import cde.{Parameters, Field}

case object TLId extends Field[String]
case class TLKey(id: String) extends Field[TileLinkParameters]

/** Parameters exposed to the top-level design, set based on 
  * external requirements or design space exploration
  *
  * Coherency policy used to define custom mesage types
  * Number of manager agents
  * Number of client agents that cache data and use custom [[uncore.Acquire]] types
  * Number of client agents that do not cache data and use built-in [[uncore.Acquire]] types
  * Maximum number of unique outstanding transactions per client
  * Maximum number of clients multiplexed onto a single port
  * Maximum number of unique outstanding transactions per manager
  * Width of cache block addresses
  * Total amount of data per cache block
  * Number of data beats per cache block
  **/

case class TileLinkParameters(
    coherencePolicy: CoherencePolicy,
    nManagers: Int,
    nCachingClients: Int,
    nCachelessClients: Int,
    maxClientXacts: Int,
    maxClientsPerPort: Int,
    maxManagerXacts: Int,
    dataBits: Int,
    dataBeats: Int = 4,
    overrideDataBitsPerBeat: Option[Int] = None
    ) {
  val nClients = nCachingClients + nCachelessClients
  val writeMaskBits: Int  = ((dataBits / dataBeats) - 1) / 8 + 1
  val dataBitsPerBeat: Int = overrideDataBitsPerBeat.getOrElse(dataBits / dataBeats)
}

  
/** Utility trait for building Modules and Bundles that use TileLink parameters */
trait HasTileLinkParameters {
  implicit val p: Parameters
  val tlExternal = p(TLKey(p(TLId)))
  val tlCoh = tlExternal.coherencePolicy
  val tlNManagers = tlExternal.nManagers
  val tlNCachingClients = tlExternal.nCachingClients
  val tlNCachelessClients = tlExternal.nCachelessClients
  val tlNClients = tlExternal.nClients
  val tlClientIdBits =  log2Up(tlNClients)
  val tlManagerIdBits =  log2Up(tlNManagers)
  val tlMaxClientXacts = tlExternal.maxClientXacts
  val tlMaxClientsPerPort = tlExternal.maxClientsPerPort
  val tlMaxManagerXacts = tlExternal.maxManagerXacts
  val tlClientXactIdBits = log2Up(tlMaxClientXacts*tlMaxClientsPerPort)
  val tlManagerXactIdBits = log2Up(tlMaxManagerXacts)
  val tlBlockAddrBits = p(PAddrBits) - p(CacheBlockOffsetBits)
  val tlDataBeats = tlExternal.dataBeats
  val tlDataBits = tlExternal.dataBitsPerBeat
  val tlDataBytes = tlDataBits/8
  val tlWriteMaskBits = tlExternal.writeMaskBits
  val tlBeatAddrBits = log2Up(tlDataBeats)
  val tlByteAddrBits = log2Up(tlWriteMaskBits)
  val tlMemoryOpcodeBits = M_SZ
  val tlMemoryOperandSizeBits = MT_SZ
  val tlAcquireTypeBits = max(log2Up(Acquire.nBuiltInTypes), 
                              tlCoh.acquireTypeWidth)
  val tlAcquireUnionBits = max(tlWriteMaskBits,
                                 (tlByteAddrBits +
                                   tlMemoryOperandSizeBits +
                                   tlMemoryOpcodeBits)) + 1
  val tlGrantTypeBits = max(log2Up(Grant.nBuiltInTypes), 
                              tlCoh.grantTypeWidth) + 1
/** Whether the underlying physical network preserved point-to-point ordering of messages */
  val tlNetworkPreservesPointToPointOrdering = false
  val tlNetworkDoesNotInterleaveBeats = true
  val amoAluOperandBits = p(AmoAluOperandBits)
  val amoAluOperandBytes = amoAluOperandBits/8
}

abstract class TLModule(implicit val p: Parameters) extends Module
  with HasTileLinkParameters
abstract class TLBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasTileLinkParameters

/** Base trait for all TileLink channels */
abstract class TileLinkChannel(implicit p: Parameters) extends TLBundle()(p) {
  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ClientToManagerChannel(implicit p: Parameters) extends TileLinkChannel()(p)
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ManagerToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p)
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ClientToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p) // Unused for now

/** Common signals that are used in multiple channels.
  * These traits are useful for type parameterizing bundle wiring functions.
  */

/** Address of a cache block. */
trait HasCacheBlockAddress extends HasTileLinkParameters {
  val addr_block = UInt(width = tlBlockAddrBits)

  def conflicts(that: HasCacheBlockAddress) = this.addr_block === that.addr_block
  def conflicts(addr: UInt) = this.addr_block === addr
}

/** Sub-block address or beat id of multi-beat data */
trait HasTileLinkBeatId extends HasTileLinkParameters {
  val addr_beat = UInt(width = tlBeatAddrBits)
}

/* Client-side transaction id. Usually Miss Status Handling Register File index */
trait HasClientTransactionId extends HasTileLinkParameters {
  val client_xact_id = Bits(width = tlClientXactIdBits)
}

/** Manager-side transaction id. Usually Transaction Status Handling Register File index. */
trait HasManagerTransactionId extends HasTileLinkParameters {
  val manager_xact_id = Bits(width = tlManagerXactIdBits)
}

/** A single beat of cache block data */
trait HasTileLinkData extends HasTileLinkBeatId {
  val data = UInt(width = tlDataBits)

  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}

/** An entire cache block of data */
trait HasTileLinkBlock extends HasTileLinkParameters {
  val data_buffer = Vec(tlDataBeats, UInt(width = tlDataBits))
  val wmask_buffer = Vec(tlDataBeats, UInt(width = tlWriteMaskBits))
}

/** The id of a client source or destination. Used in managers. */
trait HasClientId extends HasTileLinkParameters {
  val client_id = UInt(width = tlClientIdBits)
}

trait HasAcquireUnion extends HasTileLinkParameters {
  val union = Bits(width = tlAcquireUnionBits)

  // Utility funcs for accessing subblock union:
  def isBuiltInType(t: UInt): Bool
  val opCodeOff = 1
  val opSizeOff = tlMemoryOpcodeBits + opCodeOff
  val addrByteOff = tlMemoryOperandSizeBits + opSizeOff
  val addrByteMSB = tlByteAddrBits + addrByteOff
  /** Hint whether to allocate the block in any interveneing caches */
  def allocate(dummy: Int = 0) = union(0)
  /** Op code for [[uncore.PutAtomic]] operations */
  def op_code(dummy: Int = 0) = Mux(
    isBuiltInType(Acquire.putType) || isBuiltInType(Acquire.putBlockType),
    M_XWR, union(opSizeOff-1, opCodeOff))
  /** Operand size for [[uncore.PutAtomic]] */
  def op_size(dummy: Int = 0) = union(addrByteOff-1, opSizeOff)
  /** Byte address for [[uncore.PutAtomic]] operand */
  def addr_byte(dummy: Int = 0) = union(addrByteMSB-1, addrByteOff)
  def amo_offset(dummy: Int = 0) = addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBytes))
  /** Bit offset of [[uncore.PutAtomic]] operand */
  def amo_shift_bytes(dummy: Int = 0) = UInt(amoAluOperandBytes)*amo_offset()
  /** Write mask for [[uncore.Put]], [[uncore.PutBlock]], [[uncore.PutAtomic]] */
  def wmask(dummy: Int = 0): UInt = {
    val amo_word_mask =
      if (amoAluOperandBytes == tlWriteMaskBits) UInt(1)
      else UIntToOH(amo_offset())
    Mux(isBuiltInType(Acquire.putAtomicType), 
      FillInterleaved(amoAluOperandBytes, amo_word_mask),
      Mux(isBuiltInType(Acquire.putBlockType) || isBuiltInType(Acquire.putType),
        union(tlWriteMaskBits, 1),
        UInt(0, width = tlWriteMaskBits)))
  }
  /** Full, beat-sized writemask */
  def full_wmask(dummy: Int = 0) = FillInterleaved(8, wmask())
}

trait HasAcquireType extends HasTileLinkParameters {
  val is_builtin_type = Bool()
  val a_type = UInt(width = tlAcquireTypeBits)

  /** Message type equality */
  def is(t: UInt) = a_type === t //TODO: make this more opaque; def ===?

  /** Is this message a built-in or custom type */
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  /** Is this message a particular built-in type */
  def isBuiltInType(t: UInt): Bool = is_builtin_type && a_type === t 

  /** Does this message refer to subblock operands using info in the Acquire.union subbundle */ 
  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  /** Is this message a built-in prefetch message */
  def isPrefetch(dummy: Int = 0): Bool = isBuiltInType() &&
                                           (is(Acquire.getPrefetchType) || is(Acquire.putPrefetchType))

  /** Does this message contain data? Assumes that no custom message types have data. */
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  /** Does this message contain multiple beats of data? Assumes that no custom message types have data. */
  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  /** Does this message require the manager to probe the client the very client that sent it?
    * Needed if multiple caches are attached to the same port.
    */
  def requiresSelfProbe(dummy: Int = 0) = Bool(false)

  /** Mapping between each built-in Acquire type and a built-in Grant type.  */
  def getBuiltInGrantType(dummy: Int = 0): UInt = Acquire.getBuiltInGrantType(this.a_type)
}

trait HasProbeType extends HasTileLinkParameters {
  val p_type = UInt(width = tlCoh.probeTypeWidth)

  def is(t: UInt) = p_type === t
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

trait HasReleaseType extends HasTileLinkParameters {
  val voluntary = Bool()
  val r_type = UInt(width = tlCoh.releaseTypeWidth)

  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = tlCoh.releaseTypesWithData.contains(r_type)
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1) &&
                                           tlCoh.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !Bool(tlNetworkPreservesPointToPointOrdering)
}

trait HasGrantType extends HasTileLinkParameters {
  val is_builtin_type = Bool()
  val g_type = UInt(width = tlGrantTypeBits)

  // Helper funcs
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  def isBuiltInType(t: UInt): Bool = is_builtin_type && g_type === t 
  def is(t: UInt):Bool = g_type === t
  def hasData(dummy: Int = 0): Bool = Mux(isBuiltInType(),
                                        Grant.typesWithData.contains(g_type),
                                        tlCoh.grantTypesWithData.contains(g_type))
  def hasMultibeatData(dummy: Int = 0): Bool = 
    Bool(tlDataBeats > 1) && Mux(isBuiltInType(),
                               Grant.typesWithMultibeatData.contains(g_type),
                               tlCoh.grantTypesWithData.contains(g_type))
  def isVoluntary(dummy: Int = 0): Bool = isBuiltInType() && (g_type === Grant.voluntaryAckType)
  def requiresAck(dummy: Int = 0): Bool = !Bool(tlNetworkPreservesPointToPointOrdering) && !isVoluntary()
}

/** TileLink channel bundle definitions */

/** The Acquire channel is used to intiate coherence protocol transactions in
  * order to gain access to a cache block's data with certain permissions
  * enabled. Messages sent over this channel may be custom types defined by
  * a [[uncore.CoherencePolicy]] for cached data accesse or may be built-in types
  * used for uncached data accesses. Acquires may contain data for Put or
  * PutAtomic built-in types. After sending an Acquire, clients must
  * wait for a manager to send them a [[uncore.Grant]] message in response.
  */
class AcquireMetadata(implicit p: Parameters) extends ClientToManagerChannel
    with HasCacheBlockAddress 
    with HasClientTransactionId
    with HasTileLinkBeatId
    with HasAcquireType
    with HasAcquireUnion {
  /** Complete physical address for block, beat or operand */
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())
}

/** [[uncore.AcquireMetadata]] with an extra field containing the data beat */
class Acquire(implicit p: Parameters) extends AcquireMetadata
  with HasTileLinkData

/** [[uncore.AcquireMetadata]] with an extra field containing the entire cache block */
class BufferedAcquire(implicit p: Parameters) extends AcquireMetadata
  with HasTileLinkBlock

/** [[uncore.Acquire]] with an extra field stating its source id */
class AcquireFromSrc(implicit p: Parameters) extends Acquire
  with HasClientId

/** [[uncore.BufferedAcquire]] with an extra field stating its source id */
class BufferedAcquireFromSrc(implicit p: Parameters) extends BufferedAcquire
  with HasClientId 

/** Used to track metadata for transactions where multiple secondary misses have been merged
  * and handled by a single transaction tracker.
  */
class SecondaryMissInfo(implicit p: Parameters) extends TLBundle
  with HasClientTransactionId
  with HasTileLinkBeatId
  with HasClientId
  with HasAcquireType

/** Contains definitions of the the built-in Acquire types and a factory
  * for [[uncore.Acquire]]
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeAcquire]] for custom cached Acquires and
  * [[uncore.Get]], [[uncore.Put]], etc. for built-in uncached Acquires.
  *
  * @param is_builtin_type built-in or custom type message?
  * @param a_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param data data being put outwards
  * @param union additional fields used for uncached types
  */
object Acquire {
  val nBuiltInTypes = 5
  //TODO: Use Enum
  def getType         = UInt("b000") // Get a single beat of data
  def getBlockType    = UInt("b001") // Get a whole block of data
  def putType         = UInt("b010") // Put a single beat of data
  def putBlockType    = UInt("b011") // Put a whole block of data
  def putAtomicType   = UInt("b100") // Perform an atomic memory op
  def getPrefetchType = UInt("b101") // Prefetch a whole block of data
  def putPrefetchType = UInt("b110") // Prefetch a whole block of data, with intent to write
  def typesWithData = Vec(putType, putBlockType, putAtomicType)
  def typesWithMultibeatData = Vec(putBlockType)
  def typesOnSubBlocks = Vec(putType, getType, putAtomicType)

  /** Mapping between each built-in Acquire type and a built-in Grant type. */
  def getBuiltInGrantType(a_type: UInt): UInt = {
    MuxLookup(a_type, Grant.putAckType, Array(
      Acquire.getType       -> Grant.getDataBeatType,
      Acquire.getBlockType  -> Grant.getDataBlockType,
      Acquire.putType       -> Grant.putAckType,
      Acquire.putBlockType  -> Grant.putAckType,
      Acquire.putAtomicType -> Grant.getDataBeatType,
      Acquire.getPrefetchType -> Grant.prefetchAckType,
      Acquire.putPrefetchType -> Grant.prefetchAckType))
  }

  def makeUnion(
        a_type: UInt,
        addr_byte: UInt,
        operand_size: UInt,
        opcode: UInt,
        wmask: UInt,
        alloc: Bool): UInt = {
    MuxLookup(a_type, UInt(0), Array(
      Acquire.getType       -> Cat(addr_byte, operand_size, opcode, alloc),
      Acquire.getBlockType  -> Cat(operand_size, opcode, alloc),
      Acquire.putType       -> Cat(wmask, alloc),
      Acquire.putBlockType  -> Cat(wmask, alloc),
      Acquire.putAtomicType -> Cat(addr_byte, operand_size, opcode, alloc),
      Acquire.getPrefetchType -> Cat(M_XRD, alloc),
      Acquire.putPrefetchType -> Cat(M_XWR, alloc)))
  }

  def fullWriteMask(implicit p: Parameters) = SInt(-1, width = p(TLKey(p(TLId))).writeMaskBits).toUInt

  // Most generic constructor
  def apply(
        is_builtin_type: Bool,
        a_type: Bits,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0),
        union: UInt = UInt(0))
      (implicit p: Parameters): Acquire = {
    val acq = Wire(new Acquire)
    acq.is_builtin_type := is_builtin_type
    acq.a_type := a_type
    acq.client_xact_id := client_xact_id
    acq.addr_block := addr_block
    acq.addr_beat := addr_beat
    acq.data := data
    acq.union := union
    acq
  }

  // Copy constructor
  def apply(a: Acquire): Acquire = {
    val acq = Wire(new Acquire()(a.p))
    acq := a
    acq
  }
}

object BuiltInAcquireBuilder {
  def apply(
        a_type: UInt,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0),
        addr_byte: UInt = UInt(0),
        operand_size: UInt = MT_Q,
        opcode: UInt = UInt(0),
        wmask: UInt = UInt(0),
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    Acquire(
        is_builtin_type = Bool(true),
        a_type = a_type,
        client_xact_id = client_xact_id,
        addr_block = addr_block,
        addr_beat = addr_beat,
        data = data,
        union = Acquire.makeUnion(a_type, addr_byte, operand_size, opcode, wmask, alloc))
  }
}

/** Get a single beat of data from the outer memory hierarchy
  *
  * The client can hint whether he block containing this beat should be 
  * allocated in the intervening levels of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param addr_byte sub-block address (which byte)
  * @param operand_size {byte, half, word, double} from [[uncore.MemoryOpConstants]]
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Get {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      opcode = M_XRD,
      alloc = alloc)
  }
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        addr_byte: UInt,
        operand_size: UInt,
        alloc: Bool)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      addr_byte = addr_byte, 
      operand_size = operand_size,
      opcode = M_XRD,
      alloc = alloc)
  }
}

/** Get a whole cache block of data from the outer memory hierarchy
  *
  * The client can hint whether the block should be allocated in the 
  * intervening levels of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object GetBlock {
  def apply(
        client_xact_id: UInt = UInt(0),
        addr_block: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getBlockType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block,
      opcode = M_XRD,
      alloc = alloc)
  }
}

/** Prefetch a cache block into the next-outermost level of the memory hierarchy
  * with read permissions.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  */
object GetPrefetch {
  def apply(
       client_xact_id: UInt,
       addr_block: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getPrefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block)
  }
}

/** Put a single beat of data into the outer memory hierarchy
  *
  * The block will be allocated in the next-outermost level of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param data data being refilled to the original requestor
  * @param wmask per-byte write mask for this beat
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Put {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        wmask: Option[UInt]= None,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putType,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      wmask = wmask.getOrElse(Acquire.fullWriteMask),
      alloc = alloc)
  }
}

/** Put a whole cache block of data into the outer memory hierarchy
  *
  * If the write mask is not full, the block will be allocated in the
  * next-outermost level of the hierarchy. If the write mask is full, the
  * client can hint whether the block should be allocated or not.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat of several)
  * @param data data being refilled to the original requestor
  * @param wmask per-byte write mask for this beat
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object PutBlock {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        wmask: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      wmask = wmask,
      alloc = Bool(true))
  }
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      wmask = Acquire.fullWriteMask,
      alloc = alloc)
  }
}

/** Prefetch a cache block into the next-outermost level of the memory hierarchy
  * with write permissions.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  */
object PutPrefetch {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putPrefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block)
  }
}

/** Perform an atomic memory operation in the next-outermost level of the memory hierarchy
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (within which beat)
  * @param addr_byte sub-block address (which byte)
  * @param atomic_opcode {swap, add, xor, and, min, max, minu, maxu} from [[uncore.MemoryOpConstants]]
  * @param operand_size {byte, half, word, double} from [[uncore.MemoryOpConstants]]
  * @param data source operand data
  */
object PutAtomic {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        addr_byte: UInt,
        atomic_opcode: UInt,
        operand_size: UInt,
        data: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putAtomicType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block, 
      addr_beat = addr_beat, 
      data = data,
      addr_byte = addr_byte,
      operand_size = operand_size,
      opcode = atomic_opcode)
  }
}

/** The Probe channel is used to force clients to release data or cede permissions
  * on a cache block. Clients respond to Probes with [[uncore.Release]] messages.
  * The available types of Probes are customized by a particular
  * [[uncore.CoherencePolicy]].
  */
class Probe(implicit p: Parameters) extends ManagerToClientChannel
  with HasCacheBlockAddress 
  with HasProbeType

/** [[uncore.Probe]] with an extra field stating its destination id */
class ProbeToDst(implicit p: Parameters) extends Probe()(p) with HasClientId

/** Contains factories for [[uncore.Probe]] and [[uncore.ProbeToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeProbe(UInt,Acquire)* makeProbe]] instead.
  *
  * @param dst id of client to which probe should be sent
  * @param p_type custom probe type
  * @param addr_block address of the cache block
  */
object Probe {
  def apply(p_type: UInt, addr_block: UInt)(implicit p: Parameters): Probe = {
    val prb = Wire(new Probe)
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
  def apply(dst: UInt, p_type: UInt, addr_block: UInt)(implicit p: Parameters): ProbeToDst = {
    val prb = Wire(new ProbeToDst)
    prb.client_id := dst
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
}

/** The Release channel is used to release data or permission back to the manager
  * in response to [[uncore.Probe]] messages. It can also be used to voluntarily
  * write back data, for example in the event that dirty data must be evicted on
  * a cache miss. The available types of Release messages are always customized by
  * a particular [[uncore.CoherencePolicy]]. Releases may contain data or may be
  * simple acknowledgements. Voluntary Releases are acknowledged with [[uncore.Grant Grants]].
  */
class ReleaseMetadata(implicit p: Parameters) extends ClientToManagerChannel
    with HasTileLinkBeatId
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasReleaseType {
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, UInt(0, width = tlByteAddrBits))
}

/** [[uncore.ReleaseMetadata]] with an extra field containing the data beat */
class Release(implicit p: Parameters) extends ReleaseMetadata
  with HasTileLinkData

/** [[uncore.ReleaseMetadata]] with an extra field containing the entire cache block */
class BufferedRelease(implicit p: Parameters) extends ReleaseMetadata
  with HasTileLinkBlock

/** [[uncore.Release]] with an extra field stating its source id */
class ReleaseFromSrc(implicit p: Parameters) extends Release
  with HasClientId

/** [[uncore.BufferedRelease]] with an extra field stating its source id */
class BufferedReleaseFromSrc(implicit p: Parameters) extends BufferedRelease
  with HasClientId

/** Contains a [[uncore.Release]] factory
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeRelease]] instead.
  *
  * @param voluntary is this a voluntary writeback
  * @param r_type type enum defined by coherence protocol
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat beat id of the data
  * @param data data being written back
  */
object Release {
  def apply(
        voluntary: Bool,
        r_type: UInt,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0))
      (implicit p: Parameters): Release = {
    val rel = Wire(new Release)
    rel.r_type := r_type
    rel.client_xact_id := client_xact_id
    rel.addr_block := addr_block
    rel.addr_beat := addr_beat
    rel.data := data
    rel.voluntary := voluntary
    rel
  }
}

/** The Grant channel is used to refill data or grant permissions requested of the 
  * manager agent via an [[uncore.Acquire]] message. It is also used to acknowledge
  * the receipt of voluntary writeback from clients in the form of [[uncore.Release]]
  * messages. There are built-in Grant messages used for Gets and Puts, and
  * coherence policies may also define custom Grant types. Grants may contain data
  * or may be simple acknowledgements. Grants are responded to with [[uncore.Finish]].
  */
class GrantMetadata(implicit p: Parameters) extends ManagerToClientChannel
    with HasTileLinkBeatId
    with HasClientTransactionId 
    with HasManagerTransactionId
    with HasGrantType {
  def makeFinish(dummy: Int = 0): Finish = {
    val f = Wire(new Finish)
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

/** [[uncore.GrantMetadata]] with an extra field containing a single beat of data */
class Grant(implicit p: Parameters) extends GrantMetadata
  with HasTileLinkData

/** [[uncore.Grant]] with an extra field stating its destination */
class GrantToDst(implicit p: Parameters) extends Grant
  with HasClientId

/** [[uncore.GrantMetadata]] with an extra field containing an entire cache block */
class BufferedGrant(implicit p: Parameters) extends GrantMetadata
  with HasTileLinkBlock

/** [[uncore.BufferedGrant]] with an extra field stating its destination */
class BufferedGrantToDst(implicit p: Parameters) extends BufferedGrant
  with HasClientId

/** Contains definitions of the the built-in grant types and factories 
  * for [[uncore.Grant]] and [[uncore.GrantToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeGrant(uncore.AcquireFromSrc* makeGrant]] instead.
  *
  * @param dst id of client to which grant should be sent
  * @param is_builtin_type built-in or custom type message?
  * @param g_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param manager_xact_id manager's transaction id
  * @param addr_beat beat id of the data
  * @param data data being refilled to the original requestor
  */
object Grant {
  val nBuiltInTypes = 5
  def voluntaryAckType = UInt("b000") // For acking Releases
  def prefetchAckType  = UInt("b001") // For acking any kind of Prefetch
  def putAckType       = UInt("b011") // For acking any kind of non-prfetch Put
  def getDataBeatType  = UInt("b100") // Supplying a single beat of Get
  def getDataBlockType = UInt("b101") // Supplying all beats of a GetBlock
  def typesWithData = Vec(getDataBlockType, getDataBeatType)
  def typesWithMultibeatData= Vec(getDataBlockType)

  def apply(
        is_builtin_type: Bool,
        g_type: UInt,
        client_xact_id: UInt, 
        manager_xact_id: UInt,
        addr_beat: UInt,
        data: UInt)
      (implicit p: Parameters): Grant = {
    val gnt = Wire(new Grant)
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }

  def apply(
        dst: UInt,
        is_builtin_type: Bool,
        g_type: UInt,
        client_xact_id: UInt,
        manager_xact_id: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0))
      (implicit p: Parameters): GrantToDst = {
    val gnt = Wire(new GrantToDst)
    gnt.client_id := dst
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }
}

/** The Finish channel is used to provide a global ordering of transactions
  * in networks that do not guarantee point-to-point ordering of messages.
  * A Finsish message is sent as acknowledgement of receipt of a [[uncore.Grant]].
  * When a Finish message is received, a manager knows it is safe to begin
  * processing other transactions that touch the same cache block.
  */
class Finish(implicit p: Parameters) extends ClientToManagerChannel()(p)
    with HasManagerTransactionId {
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

/** Complete IO definition for incoherent TileLink, including networking headers */
class UncachedTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new Acquire))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val finish = new DecoupledIO(new LogicalNetworkIO(new Finish))
}

/** Complete IO definition for coherent TileLink, including networking headers */
class TileLinkIO(implicit p: Parameters) extends UncachedTileLinkIO()(p) {
  val probe     = new DecoupledIO(new LogicalNetworkIO(new Probe)).flip
  val release   = new DecoupledIO(new LogicalNetworkIO(new Release))
}

/** This version of UncachedTileLinkIO does not contain network headers. 
  * It is intended for use within client agents.
  *
  * Headers are provided in the top-level that instantiates the clients and network,
  * probably using a [[uncore.ClientTileLinkNetworkPort]] module.
  * By eliding the header subbundles within the clients we can enable 
  * hierarchical P-and-R while minimizing unconnected port errors in GDS.
  *
  * Secondly, this version of the interface elides [[uncore.Finish]] messages, with the
  * assumption that a [[uncore.FinishUnit]] has been coupled to the TileLinkIO port
  * to deal with acking received [[uncore.Grant Grants]].
  */
class ClientUncachedTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new Acquire)
  val grant     = new DecoupledIO(new Grant).flip
}

/** This version of TileLinkIO does not contain network headers. 
  * It is intended for use within client agents.
  */
class ClientTileLinkIO(implicit p: Parameters) extends ClientUncachedTileLinkIO()(p) {
  val probe     = new DecoupledIO(new Probe).flip
  val release   = new DecoupledIO(new Release)
}

/** This version of TileLinkIO does not contain network headers, but
  * every channel does include an extra client_id subbundle.
  * It is intended for use within Management agents.
  *
  * Managers need to track where [[uncore.Acquire]] and [[uncore.Release]] messages
  * originated so that they can send a [[uncore.Grant]] to the right place. 
  * Similarly they must be able to issues Probes to particular clients.
  * However, we'd still prefer to have [[uncore.ManagerTileLinkNetworkPort]] fill in
  * the header.src to enable hierarchical p-and-r of the managers. Additionally, 
  * coherent clients might be mapped to random network port ids, and we'll leave it to the
  * [[uncore.ManagerTileLinkNetworkPort]] to apply the correct mapping. Managers do need to
  * see Finished so they know when to allow new transactions on a cache
  * block to proceed.
  */
class ManagerTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new AcquireFromSrc).flip
  val grant     = new DecoupledIO(new GrantToDst)
  val finish    = new DecoupledIO(new Finish).flip
  val probe     = new DecoupledIO(new ProbeToDst)
  val release   = new DecoupledIO(new ReleaseFromSrc).flip
}

/** Struct for describing per-channel queue depths */
case class TileLinkDepths(acq: Int, prb: Int, rel: Int, gnt: Int, fin: Int)

/** Optionally enqueues each [[uncore.TileLinkChannel]] individually */
class TileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val client = new TileLinkIO().flip
    val manager = new TileLinkIO
  }
  io.manager.acquire <> (if(depths.acq > 0) Queue(io.client.acquire, depths.acq) else io.client.acquire)
  io.client.probe    <> (if(depths.prb > 0) Queue(io.manager.probe,  depths.prb) else io.manager.probe)
  io.manager.release <> (if(depths.rel > 0) Queue(io.client.release, depths.rel) else io.client.release)
  io.client.grant    <> (if(depths.gnt > 0) Queue(io.manager.grant,  depths.gnt) else io.manager.grant)
  io.manager.finish  <> (if(depths.fin > 0) Queue(io.client.finish,  depths.fin) else io.client.finish)
}

object TileLinkEnqueuer {
  def apply(in: TileLinkIO, depths: TileLinkDepths)(implicit p: Parameters): TileLinkIO = {
    val t = Module(new TileLinkEnqueuer(depths))
    t.io.client <> in
    t.io.manager
  }
  def apply(in: TileLinkIO, depth: Int)(implicit p: Parameters): TileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }
}

class ClientTileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val inner = new ClientTileLinkIO().flip
    val outer = new ClientTileLinkIO
  }

  io.outer.acquire <> (if(depths.acq > 0) Queue(io.inner.acquire, depths.acq) else io.inner.acquire)
  io.inner.probe   <> (if(depths.prb > 0) Queue(io.outer.probe,   depths.prb) else io.outer.probe)
  io.outer.release <> (if(depths.rel > 0) Queue(io.inner.release, depths.rel) else io.inner.release)
  io.inner.grant   <> (if(depths.gnt > 0) Queue(io.outer.grant,   depths.gnt) else io.outer.grant)
}

object ClientTileLinkEnqueuer {
  def apply(in: ClientTileLinkIO, depths: TileLinkDepths)(implicit p: Parameters): ClientTileLinkIO = {
    val t = Module(new ClientTileLinkEnqueuer(depths))
    t.io.inner <> in
    t.io.outer
  }
  def apply(in: ClientTileLinkIO, depth: Int)(implicit p: Parameters): ClientTileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }
}

/** Utility functions for constructing TileLinkIO arbiters */
trait TileLinkArbiterLike extends HasTileLinkParameters {
  // Some shorthand type variables
  type ManagerSourcedWithId = ManagerToClientChannel with HasClientTransactionId
  type ClientSourcedWithId = ClientToManagerChannel with HasClientTransactionId
  type ClientSourcedWithIdAndData = ClientToManagerChannel with HasClientTransactionId with HasTileLinkData

  val arbN: Int // The number of ports on the client side

  // These abstract funcs are filled in depending on whether the arbiter mucks with the 
  // outgoing client ids to track sourcing and then needs to revert them on the way back
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def managerSourcedClientXactId(in: ManagerSourcedWithId): Bits
  def arbIdx(in: ManagerSourcedWithId): UInt

  // The following functions are all wiring helpers for each of the different types of TileLink channels

  def hookupClientSource[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]],
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = m.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupClientSourceHeaderless[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[M]],
      mngr: DecoupledIO[M]) {
    def hasData(m: M) = m.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.client_xact_id := clientSourcedClientXactId(req.bits, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupManagerSourceWithHeader[M <: ManagerToClientChannel](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (mngr.bits.header.dst === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
    }
  }

  def hookupManagerSourceWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits.payload) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.payload.client_xact_id := managerSourcedClientXactId(mngr.bits.payload)
    }
  }

  def hookupManagerSourceHeaderlessWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[M]], 
      mngr: DecoupledIO[M]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.client_xact_id := managerSourcedClientXactId(mngr.bits)
    }
  }

  def hookupManagerSourceBroadcast[M <: Data](clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    clts.map{ _.valid := mngr.valid }
    clts.map{ _.bits := mngr.bits }
    mngr.ready := clts.map(_.ready).reduce(_&&_)
  }

  def hookupFinish[M <: LogicalNetworkIO[Finish]]( clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    val arb = Module(new RRArbiter(mngr.bits, arbN))
    arb.io.in <> clts
    mngr <> arb.io.out
  }
}

/** Abstract base case for any Arbiters that have UncachedTileLinkIOs */
abstract class UncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(arbN, new UncachedTileLinkIO).flip
    val out = new UncachedTileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Abstract base case for any Arbiters that have cached TileLinkIOs */
abstract class TileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(arbN, new TileLinkIO).flip
    val out = new TileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupClientSource(io.in.map(_.release), io.out.release)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Appends the port index of the arbiter to the client_xact_id */
trait AppendsArbiterId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = 
    in.client_xact_id >> log2Up(arbN)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id(log2Up(arbN)-1,0).toUInt
}

/** Uses the client_xact_id as is (assumes it has been set to port index) */
trait PassesId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = in.client_xact_id
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = in.client_xact_id
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

/** Overwrites some default client_xact_id with the port idx */
trait UsesNewId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = UInt(id, log2Up(arbN))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = UInt(0)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

// Now we can mix-in thevarious id-generation traits to make concrete arbiter classes
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with UsesNewId

/** Concrete uncached client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientUncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(arbN, new ClientUncachedTileLinkIO).flip
    val out = new ClientUncachedTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Concrete client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(arbN, new ClientTileLinkIO).flip
    val out = new ClientTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupClientSourceHeaderless(io.in.map(_.release), io.out.release)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}
/** A super channel message container that can contain all kind of channels
  * Potential useful in a crossbar shared by all channels
  */
class SuperChannel(implicit p: Parameters) extends TileLinkChannel()(p)
    with HasCacheBlockAddress
    with HasTileLinkData
    with HasClientTransactionId
    with HasManagerTransactionId
    with HasClientId
{
  val flag = Bool() // Acquire::is_builtin_tye || Grant::is_builtin_tye || Release::voluntary
  val hasMultibeatData = Bool() // TODO: current toAcquire, etc. does not work

  // a_type || r_type || g_type
  val mtype = UInt(width = 
    Array(tlAcquireTypeBits, tlCoh.probeTypeWidth, tlCoh.releaseTypeWidth, tlGrantTypeBits).reduceLeft(_ max _))

  val union = Bits(width = tlAcquireUnionBits)
  val ctype = UInt(width=3) // type of the channel (Acquire, Probe, Release, Grant, Finish)

  // implement virtual helper functions derived from TileLinkChannel
  def hasData(dummy: Int = 0): Bool = {
    MuxLookup(ctype, Bool(false), Array(
      SuperChannel.acquireType  -> toAcquire().hasData(),
      SuperChannel.probeType    -> toProbe().hasData(),
      SuperChannel.releaseType  -> toRelease().hasData(),
      SuperChannel.grantType    -> toGrant().hasData(),
      SuperChannel.finishType   -> toFinish().hasData()
    ))
  }

  def hasMultibeatData(dummy: Int = 0): Bool = {
    // TODO: No idea why toAcquire etc. does not work
    /*
    MuxLookup(ctype, Bool(false), Array(
      SuperChannel.acquireType  -> toAcquire().hasMultibeatData(),
      SuperChannel.probeType    -> toProbe().hasMultibeatData(),
      SuperChannel.releaseType  -> toRelease().hasMultibeatData(),
      SuperChannel.grantType    -> toGrant().hasMultibeatData(),
      SuperChannel.finishType   -> toFinish().hasMultibeatData()
    ))
     */
    hasMultibeatData
  }

  // conversion helpers
  def toAcquire(dummy: Int = 0) = 
    Acquire(flag, mtype, client_xact_id, addr_block, addr_beat, data, union)
  def toProbe(dummy: Int = 0) =
    Probe(mtype, addr_block)
  def toRelease(dummy: Int = 0) =
    Release(flag, mtype, client_xact_id, addr_block, addr_beat, data)
  def toGrant(dummy: Int = 0) =
    Grant(flag, mtype, client_xact_id, manager_xact_id, addr_beat, data)
  def toFinish(dummy: Int = 0): Finish = {
    val fin = Wire(new Finish)
    fin.manager_xact_id := manager_xact_id
    fin
  }

  // type checker
  def isAcquire(dummy: Int = 0): Bool = ctype === SuperChannel.acquireType
  def isProbe(dummy: Int = 0): Bool = ctype === SuperChannel.probeType
  def isRelease(dummy: Int = 0): Bool = ctype === SuperChannel.releaseType
  def isGrant(dummy: Int = 0): Bool = ctype === SuperChannel.grantType
  def isFinish(dummy: Int = 0): Bool = ctype === SuperChannel.finishType
}

object SuperChannel {
  // channel types
  def acquireType = UInt("b000")
  def probeType   = UInt("b001")
  def releaseType = UInt("b010")
  def grantType   = UInt("b011")
  def finishType  = UInt("b100")

  // Acquire => SuperChannel
  def apply(acq: Acquire)(implicit p: Parameters): SuperChannel = {
    val msg = Wire(new SuperChannel)
    msg.ctype := acquireType
    msg.flag := acq.is_builtin_type
    msg.mtype := acq.a_type
    msg.client_xact_id := acq.client_xact_id
    msg.addr_block := acq.addr_block
    msg.addr_beat := acq.addr_beat
    msg.data := acq.data
    msg.union := acq.union
    msg.hasMultibeatData := acq.hasMultibeatData()
    msg
  }

  // Probe => SuperChannel
  def apply(prb: Probe)(implicit p: Parameters): SuperChannel = {
    val msg = Wire(new SuperChannel)
    msg.ctype := probeType
    msg.mtype := prb.p_type
    msg.addr_block := prb.addr_block
    msg.hasMultibeatData := prb.hasMultibeatData()
    msg
  }

  // Release => SuperChannel
  def apply(rel: Release)(implicit p: Parameters): SuperChannel = {
    val msg = Wire(new SuperChannel)
    msg.ctype := releaseType
    msg.mtype := rel.r_type
    msg.client_xact_id := rel.client_xact_id
    msg.addr_block := rel.addr_block
    msg.addr_beat := rel.addr_beat
    msg.data := rel.data
    msg.flag := rel.voluntary
    msg.hasMultibeatData := rel.hasMultibeatData()
    msg
  }

  // Grant => SuperChannel
  def apply(gnt: Grant)(implicit p: Parameters): SuperChannel = {
    val msg = Wire(new SuperChannel)
    msg.ctype := grantType
    msg.flag := gnt.is_builtin_type
    msg.mtype := gnt.g_type
    msg.client_xact_id := gnt.client_xact_id
    msg.manager_xact_id := gnt.manager_xact_id
    msg.addr_beat := gnt.addr_beat
    msg.data := gnt.data
    msg.hasMultibeatData := gnt.hasMultibeatData()
    msg
  }

  // Finish => SuperChannel
  def apply(fin: Finish)(implicit p: Parameters): SuperChannel = {
    val msg = Wire(new SuperChannel)
    msg.ctype := finishType
    msg.manager_xact_id := fin.manager_xact_id
    msg.hasMultibeatData := fin.hasMultibeatData()
    msg
  }

}

/** Super channel multiplexer to arbitrate all channels to
  * the shared super channel according to the defined priorities.
  */
class SuperChannelInputMultiplexer(implicit p: Parameters)
    extends TLModule()(p)
{
  val io = new Bundle {
    val tl = new TileLinkIO().flip
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel))
  }


  def hasData(m: LogicalNetworkIO[SuperChannel]) = m.payload.hasMultibeatData()
  val arb = Module(new LockingArbiter(io.su.bits.clone, 3, tlDataBeats, Some(hasData _), true))

  arb.io.in(0).valid := io.tl.finish.valid
  arb.io.in(0).bits.header := io.tl.finish.bits.header
  arb.io.in(0).bits.payload := SuperChannel(io.tl.finish.bits.payload)
  io.tl.finish.ready := arb.io.in(0).ready

  arb.io.in(1).valid := io.tl.release.valid
  arb.io.in(1).bits.header := io.tl.release.bits.header
  arb.io.in(1).bits.payload := SuperChannel(io.tl.release.bits.payload)
  io.tl.release.ready := arb.io.in(1).ready

  arb.io.in(2).valid := io.tl.acquire.valid
  arb.io.in(2).bits.header := io.tl.acquire.bits.header
  arb.io.in(2).bits.payload := SuperChannel(io.tl.acquire.bits.payload)
  io.tl.acquire.ready := arb.io.in(2).ready

  arb.io.out <> io.su

}

class SuperChannelOutputMultiplexer(implicit p: Parameters)
    extends TLModule()(p)
{
  val io = new Bundle {
    val tl = new TileLinkIO
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel))
  }

  def hasData(m: LogicalNetworkIO[SuperChannel]) = m.payload.hasMultibeatData()
  val arb = Module(new LockingArbiter(io.su.bits.clone, 2, tlDataBeats, Some(hasData _), true))

  arb.io.in(0).valid := io.tl.grant.valid
  arb.io.in(0).bits.header := io.tl.grant.bits.header
  arb.io.in(0).bits.payload := SuperChannel(io.tl.grant.bits.payload)
  io.tl.grant.ready := arb.io.in(0).ready

  arb.io.in(1).valid := io.tl.probe.valid
  arb.io.in(1).bits.header := io.tl.probe.bits.header
  arb.io.in(1).bits.payload := SuperChannel(io.tl.probe.bits.payload)
  io.tl.probe.ready := arb.io.in(1).ready

  arb.io.out <> io.su
}

/** Super channel demultiplexer to route super channel to corresponding
  * TileLink channels.
  */
class SuperChannelInputDemultiplexer(implicit p: Parameters)
    extends TLModule()(p)
{
  val io = new Bundle {
    val tl = new TileLinkIO().flip
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel)).flip
  }

  io.tl.grant.valid := io.su.valid && io.su.bits.payload.isGrant()
  io.tl.grant.bits.header := io.su.bits.header
  io.tl.grant.bits.payload := io.su.bits.payload.toGrant()
  
  io.tl.probe.valid := io.su.valid && io.su.bits.payload.isProbe()
  io.tl.probe.bits.header := io.su.bits.header
  io.tl.probe.bits.payload := io.su.bits.payload.toProbe()
  
  // handle the ready signals
  io.su.ready := Bool(false)

  when(io.su.valid) {
    when(io.su.bits.payload.isGrant()) {
      io.su.ready := io.tl.grant.ready
    }
    when(io.su.bits.payload.isProbe()) {
      io.su.ready := io.tl.probe.ready
    }
  }
}

class SuperChannelOutputDemultiplexer(implicit p: Parameters)
    extends TLModule()(p)
{
  val io = new Bundle {
    val tl = new TileLinkIO
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel)).flip
  }

  io.tl.finish.valid := io.su.valid && io.su.bits.payload.isFinish()
  io.tl.finish.bits.header := io.su.bits.header
  io.tl.finish.bits.payload := io.su.bits.payload.toFinish()

  io.tl.release.valid := io.su.valid && io.su.bits.payload.isRelease()
  io.tl.release.bits.header := io.su.bits.header
  io.tl.release.bits.payload := io.su.bits.payload.toRelease()

  io.tl.acquire.valid := io.su.valid && io.su.bits.payload.isAcquire()
  io.tl.acquire.bits.header := io.su.bits.header
  io.tl.acquire.bits.payload := io.su.bits.payload.toAcquire()

  // handle the ready signals
  io.su.ready := Bool(false)

  when(io.su.valid) {
    when(io.su.bits.payload.isFinish()) {
      io.su.ready := io.tl.finish.ready
    }
    when(io.su.bits.payload.isRelease()) {
      io.su.ready := io.tl.release.ready
    }
    when(io.su.bits.payload.isAcquire()) {
      io.su.ready := io.tl.acquire.ready
    }
  }
}

class NASTIMasterIOTileLinkIOConverterHandler(id: Int)(implicit p: Parameters) extends TLModule()(p) with HasNastiParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NastiIO
    val rdy = Bool(OUTPUT)
    val tl_acq_match = Bool(OUTPUT)
    val tl_rel_match = Bool(OUTPUT)
    val na_b_match = Bool(OUTPUT)
    val na_r_match = Bool(OUTPUT)
  }

  private def opSizeToXSize(ops: UInt) = MuxLookup(ops, UInt("b111"), Seq(
    MT_B  -> UInt(0),
    MT_BU -> UInt(0),
    MT_H  -> UInt(1),
    MT_HU -> UInt(1),
    MT_W  -> UInt(2),
    MT_WU -> UInt(2),
    MT_D  -> UInt(3),
    MT_Q  -> UInt(log2Up(tlDataBytes))))

  // liminations:
  val dataBits = tlDataBits*tlDataBeats 
  val dstIdBits = p(LNHeaderBits)
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(dstIdBits + tlClientXactIdBits < nastiXIdBits, "NASTIIO converter is going truncate tags: " + dstIdBits + " + " + tlClientXactIdBits + " >= " + nastiXIdBits)
  // assume MI or MEI protocol

  // rename signals
  val tl_acq = io.tl.acquire.bits
  val tl_rel = io.tl.release.bits
  val tl_gnt = io.tl.grant.bits
  val tl_fin = io.tl.finish.bits
  val na_aw = io.nasti.aw.bits
  val na_w = io.nasti.w.bits
  val na_ar = io.nasti.ar.bits
  val na_b = io.nasti.b.bits
  val na_r = io.nasti.r.bits

  // internal control signals
  val write_multiple_data = Reg(init=Bool(false))
  val read_multiple_data = Reg(init=Bool(false))
  val (nw_cnt, nw_finish) =
    Counter(io.nasti.w.fire() && write_multiple_data, tlDataBeats)
  val (nr_cnt, nr_finish) =
    Counter((io.nasti.r.fire() && read_multiple_data), tlDataBeats)
  val is_read = Reg(init=Bool(false))
  val is_write = Reg(init=Bool(false))
  val is_acq = Reg(init=Bool(false))
  val is_builtin = Reg(init=Bool(false))
  val tag_out = Reg(UInt(width = nastiXIdBits))
  val addr_out = Reg(UInt(width = nastiXAddrBits))
  val len_out = Reg(UInt(width = nastiXLenBits))
  val size_out = Reg(UInt(width = nastiXSizeBits))
  val g_type_out = Reg(UInt(width = tlGrantTypeBits))
  val cmd_sent = Reg(init=Bool(false))
  val is_idle = !(is_read || is_write)

  // signal to handler allocator
  io.rdy := is_idle
  io.tl_acq_match := tag_out === Cat(tl_acq.client_id, tl_acq.client_xact_id) && !io.rdy
  io.tl_rel_match := tag_out === Cat(tl_rel.client_id, tl_rel.client_xact_id) && !io.rdy
  io.na_b_match := na_b.id === tag_out && !io.rdy
  io.na_r_match := na_r.id === tag_out && !io.rdy

  // assigning control registers
  when(io.nasti.b.fire()) {
    write_multiple_data := Bool(false)
    is_write := Bool(false)
    cmd_sent := Bool(false)
    is_acq := Bool(false)
  }

  when(na_r.last && io.nasti.r.fire()) {
    read_multiple_data := Bool(false)
    is_read := Bool(false)
    cmd_sent := Bool(false)
    is_acq := Bool(false)
  }

  when(is_idle && io.tl.acquire.valid && !io.tl.release.valid) { // release take priority
    write_multiple_data := tl_acq.hasMultibeatData()
    read_multiple_data := !tl_acq.isBuiltInType() || tl_acq.isBuiltInType(Acquire.getBlockType)
    is_read := tl_acq.isBuiltInType() || !tl_acq.hasData()
    is_write := tl_acq.isBuiltInType() && tl_acq.hasData()
    is_acq := Bool(true)
    is_builtin := tl_acq.isBuiltInType()
    tag_out := Cat(tl_acq.client_id, tl_acq.client_xact_id)
    addr_out := Mux(tl_acq.isBuiltInType(), tl_acq.full_addr(), tl_acq.addr_block << (tlBeatAddrBits + tlByteAddrBits))
    len_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType(), UInt(tlDataBeats-1), UInt(0))
    size_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType() || tl_acq.hasData(),
                    bytesToXSize(UInt(tlDataBytes)),
                    opSizeToXSize(tl_acq.op_size()))
    g_type_out := Mux(tl_acq.isBuiltInType(), tl_acq.getBuiltInGrantType(), UInt(0)) // assume MI or MEI
  }

  when(is_idle && io.tl.release.valid) {
    write_multiple_data := Bool(true)
    read_multiple_data := Bool(false)
    is_read := Bool(false)
    is_write := Bool(true)
    is_builtin := Bool(true)
    tag_out := Cat(tl_rel.client_id, tl_rel.client_xact_id)
    addr_out := tl_rel.addr_block << (tlBeatAddrBits + tlByteAddrBits)
    len_out := UInt(tlDataBeats-1)
    size_out := bytesToXSize(UInt(tlDataBytes))
    g_type_out := Grant.voluntaryAckType
  }

  when(io.nasti.ar.fire() || io.nasti.aw.fire()) {
    cmd_sent := Bool(true)
  }

  // nasti.aw
  io.nasti.aw.valid := is_write && !cmd_sent
  na_aw.id := tag_out
  na_aw.addr := addr_out
  na_aw.len := len_out
  na_aw.size := size_out
  na_aw.burst := UInt("b01")
  na_aw.lock := Bool(false)
  na_aw.cache := UInt("b0000")
  na_aw.prot := UInt("b000")
  na_aw.qos := UInt("b0000")
  na_aw.region := UInt("b0000")
  na_aw.user := UInt(0)

  // nasti.w
  io.nasti.w.valid := ((io.tl.acquire.valid && is_acq) || (io.tl.release.valid && !is_acq)) && is_write
  na_w.strb := Mux(is_acq && tl_acq.isSubBlockType(), tl_acq.wmask(), SInt(-1, nastiWStrobeBits).toUInt)
  na_w.data := Mux(is_acq, tl_acq.data, tl_rel.data)
  na_w.last := nw_finish || is_acq && !tl_acq.hasMultibeatData()

  // nasti.ar
  io.nasti.ar.valid := is_read && !cmd_sent
  io.nasti.ar.bits := io.nasti.aw.bits

  // nasti.b
  io.nasti.b.ready := is_write && io.tl.grant.fire()

  // nasti.r
  io.nasti.r.ready := is_read && io.tl.grant.fire()

  // tilelink acquire
  io.tl.acquire.ready := is_acq && (io.nasti.w.fire() || io.nasti.ar.fire())

  // tilelink release
  io.tl.release.ready := !is_acq && io.nasti.w.fire()

  // tilelink grant
  io.tl.grant.valid := Mux(is_write, io.nasti.b.valid, io.nasti.r.valid)
  tl_gnt := Mux(is_write,
    Grant(
      dst = tag_out >> tlClientXactIdBits,
      is_builtin_type = Bool(true),
      g_type = g_type_out,
      client_xact_id = tag_out(tlClientXactIdBits-1,0),
      manager_xact_id = UInt(id)),
    Grant(
      dst = tag_out >> tlClientXactIdBits,
      is_builtin_type = is_builtin,
      g_type = g_type_out,
      client_xact_id = tag_out(tlClientXactIdBits-1,0),
      manager_xact_id = UInt(id),
      addr_beat = nr_cnt,
      data = io.nasti.r.bits.data))
}

class NastiMasterIOTileLinkIOConverter(implicit p: Parameters) extends TLModule()(p) with HasNastiParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NastiIO
  }

  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)
  io.tl.finish.ready := Bool(true)

  val handlerList = (0 until nastiHandlers).map(id => Module(new NASTIMasterIOTileLinkIOConverterHandler(id)))
  val tlAcqMatches = Vec(handlerList.map(_.io.tl_acq_match)).toBits
  val tlRelMatches = Vec(handlerList.map(_.io.tl_rel_match)).toBits
  val tlReady = Vec(handlerList.map(_.io.rdy)).toBits
  val tlAcqHandlerId = Mux(tlAcqMatches.orR,
                        PriorityEncoder(tlAcqMatches),
                        PriorityEncoder(tlReady))
  val tlRelHandlerId = Mux(tlRelMatches.orR,
                        PriorityEncoder(tlRelMatches),
                        PriorityEncoder(tlReady))
  val naBMatches = Vec(handlerList.map(_.io.na_b_match)).toBits
  val naRMatches = Vec(handlerList.map(_.io.na_r_match)).toBits
  val naBHandlerId = PriorityEncoder(naBMatches)
  val naRHandlerId = PriorityEncoder(naRMatches)

  def doInternalOutputArbitration[T <: Data](
    out: DecoupledIO[T],
    ins: Seq[DecoupledIO[T]],
    count: Int = 1,
    needsLock: Option[T => Bool] = None)
  {
    val arb = Module(new LockingRRArbiter(out.bits, ins.size, count, needsLock, true))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doInternalInputRouting[T <: Data](in: DecoupledIO[T], outs: Seq[DecoupledIO[T]], id: UInt) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && id === UInt(i) }
  }

  doInternalInputRouting(io.tl.acquire, handlerList.map(_.io.tl.acquire), tlAcqHandlerId)
  val acq_rdy = Vec(handlerList.map(_.io.tl.acquire.ready))
  io.tl.acquire.ready := (tlAcqMatches.orR || tlReady.orR) && acq_rdy(tlAcqHandlerId)

  doInternalInputRouting(io.tl.release, handlerList.map(_.io.tl.release), tlRelHandlerId)
  val rel_rdy = Vec(handlerList.map(_.io.tl.release.ready))
  io.tl.release.ready := (tlRelMatches.orR || tlReady.orR) && rel_rdy(tlRelHandlerId)

  doInternalOutputArbitration(io.tl.grant, handlerList.map(_.io.tl.grant))

  doInternalOutputArbitration(io.nasti.ar, handlerList.map(_.io.nasti.ar))

  // NASTI.w does not allow interleaving
  def w_multibeat(w: NastiWriteDataChannel): Bool = !w.last
  doInternalOutputArbitration(io.nasti.w, handlerList.map(_.io.nasti.w), tlDataBeats, w_multibeat _)

  doInternalOutputArbitration(io.nasti.aw, handlerList.map(_.io.nasti.aw))

  doInternalInputRouting(io.nasti.b, handlerList.map(_.io.nasti.b), naBHandlerId)
  val na_b_rdy = Vec(handlerList.map(_.io.nasti.b.ready))
  io.nasti.b.ready := naBMatches.orR && na_b_rdy(naBHandlerId)

  doInternalInputRouting(io.nasti.r, handlerList.map(_.io.nasti.r), naRHandlerId)
  val na_r_rdy = Vec(handlerList.map(_.io.nasti.r.ready))
  io.nasti.r.ready := naRMatches.orR && na_r_rdy(naRHandlerId)
}

class NastiLiteMasterIOTileLinkIOConverter(implicit p: Parameters) extends TLModule()(p) with HasNastiParameters with HasTileLinkParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NastiIO
  }

  // need careful revision if need to support 64-bit NASTI-Lite interface
  require(nastiXDataBits == 32)

  // request (transmit) states
  val t_idle :: t_req0 :: t_req1 :: t_busy :: Nil = Enum(UInt(), 4)
  val t_state = Reg(init=t_idle)

  // response (receiver) states
  val r_idle :: r_resp0 :: r_resp1 :: r_grant :: Nil = Enum(UInt(), 4)
  val r_state = Reg(init=r_idle)

  // internal transaction information
  val client_id = RegEnable(io.tl.acquire.bits.client_id, io.tl.acquire.valid)
  val client_xact_id = RegEnable(io.tl.acquire.bits.client_xact_id, io.tl.acquire.valid)
  val grant_type = RegEnable(io.tl.acquire.bits.getBuiltInGrantType(), io.tl.acquire.valid)
  val op_size = RegEnable(io.tl.acquire.bits.op_size(), io.tl.acquire.valid)
  val addr_high = RegEnable(io.tl.acquire.bits.addr_byte()(2), io.tl.acquire.valid)
  val data_buf = RegEnable(io.nasti.r.bits.data, r_state === r_resp0 && io.nasti.r.valid) // the higher 32-bit for 64-bit read

  // set initial values for ports
  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)

  io.nasti.aw.valid := Bool(false)
  io.nasti.w.valid := Bool(false)
  io.nasti.b.ready := Bool(false)
  io.nasti.ar.valid := Bool(false)
  io.nasti.r.ready := Bool(false)

  // drive IOs according to states

  // tl.Acquire
  io.tl.acquire.ready := t_state === t_busy && io.tl.acquire.valid // key addr and dara valid

  // tl.Grant
  io.tl.grant.valid := r_state === r_grant
  io.tl.grant.bits := Mux(grant_type === Grant.putAckType,
    Grant(client_id, Bool(true), grant_type, client_xact_id, UInt(0)),
    Grant(client_id, Bool(true), grant_type, client_xact_id, UInt(0), UInt(0),
          Cat(Mux(op_size === MT_D, io.nasti.r.bits.data, 
              Mux(addr_high, data_buf, UInt(0,32))), 
              Mux(addr_high, UInt(0,32), data_buf)))) // return data always aligns with 64-bit boundary

  // tl.Finish
  io.tl.finish.ready := Bool(true)

  // NASTI.AW
  val aw_fire = Reg(Bool())
  when((t_state === t_req0 || t_state === t_req1) && !aw_fire) {
    aw_fire := io.nasti.aw.fire()
  }
  io.nasti.aw.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.putAckType && ~aw_fire
  io.nasti.aw.bits.id := UInt(0)
  val tlNastiLiteXacts = tlDataBits / nastiXDataBits
  require(tlNastiLiteXacts > 0)
  val wmasks = Vec((0 until tlNastiLiteXacts).map(i => io.tl.acquire.bits.wmask()(i*4+3, i*4)))
  val wmasks_bv = Vec(wmasks.map(_.orR))
  val waddr_byte = PriorityEncoder(wmasks_bv.toBits)
  val waddr = Cat(io.tl.acquire.bits.addr_block, io.tl.acquire.bits.addr_beat, waddr_byte, UInt("b00"))
  val double_write = !waddr_byte(0) && wmasks_bv(waddr_byte| UInt(1))
  io.nasti.aw.bits.addr := Mux(t_state === t_req0, waddr, waddr | UInt("b100"))
  io.nasti.aw.bits.prot := UInt("b000")
  io.nasti.aw.bits.region := UInt("b0000")
  io.nasti.aw.bits.qos := UInt("b0000")
  io.nasti.aw.bits.user := UInt(0)
  
  // NASTI.W
  val w_fire = Reg(Bool())
  when((t_state === t_req0 || t_state === t_req1) && !w_fire) {
    w_fire := io.nasti.w.fire()
  }
  io.nasti.w.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.putAckType && ~w_fire

  val data_vec = Vec((0 until tlNastiLiteXacts).map(i =>
    io.tl.acquire.bits.data(i*nastiXDataBits + nastiXDataBits - 1, i*nastiXDataBits)))
  io.nasti.w.bits.data := data_vec(io.nasti.aw.bits.addr(tlByteAddrBits-1, nastiXOffBits))

  val mask_vec = Vec((0 until tlNastiLiteXacts).map(i =>
    io.tl.acquire.bits.wmask()(i*nastiWStrobeBits + nastiWStrobeBits - 1, i*nastiWStrobeBits)))
  io.nasti.w.bits.strb := mask_vec(io.nasti.aw.bits.addr(tlByteAddrBits-1, nastiXOffBits))

  // the write address and data combined fire
  val wr_fire = (io.nasti.aw.fire() || aw_fire) && (io.nasti.w.fire() || w_fire)

  // NASTI.AR
  io.nasti.ar.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.getDataBeatType
  io.nasti.ar.bits.id := UInt(0)
  val raddr = io.tl.acquire.bits.full_addr()
  io.nasti.ar.bits.addr := Mux(t_state === t_req0, raddr, raddr | UInt("b100"))
  io.nasti.ar.bits.prot := UInt("b000")
  io.nasti.ar.bits.region := UInt("b0000")
  io.nasti.ar.bits.qos := UInt("b0000")
  io.nasti.ar.bits.user := UInt(0)

  // NASTI.B
  io.nasti.b.ready := (r_state === r_resp0 || r_state === r_resp1) && grant_type === Grant.putAckType

  // NASTI.R
  when(grant_type === Grant.getDataBeatType) {
    io.nasti.r.ready := r_state === r_resp0 && op_size === MT_D ||io.tl.grant.fire()
  }

  // request state machine
  switch(t_state) {
    is(t_idle) {
      when(io.tl.acquire.valid) {
        t_state := t_req0
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_req0) {
      when(io.nasti.ar.fire()) {
        t_state := Mux(op_size === MT_D, t_req1, t_busy)
      }
      when(wr_fire) {
        t_state := Mux(double_write, t_req1, t_busy)
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_req1) {
      when(wr_fire || io.nasti.ar.fire()) {
        t_state := t_busy
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_busy) {
      when(io.tl.grant.fire()) {
        t_state := t_idle
      }
    }
  }

  // response state machine
  switch(r_state) {
    is(r_idle) {
      when(io.nasti.aw.fire() || io.nasti.ar.fire()) {
        r_state := r_resp0
      }
    }
    is(r_resp0) {
      when(io.nasti.r.valid) {
        r_state := Mux(op_size === MT_D, r_resp1, r_grant)
      }
      when(io.nasti.b.valid) {
        r_state := Mux(double_write, r_resp1, r_grant)
      }
    }
    is(r_resp1) {
      when(io.nasti.b.valid || io.nasti.r.valid) {
        r_state := r_grant
      }
    }
    is(r_grant) {
      when(io.tl.grant.valid) {
        r_state := r_idle
      }
    }
  }
}

