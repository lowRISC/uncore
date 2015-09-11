// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import ChiselComponents._
import scala.math.max


/** Utility trait for building Modules and Bundles that use TileLink parameters */
trait TileLinkParameters extends UsesParameters {
  val tlCoh = params(TLCoherencePolicy)
  val tlNManagers = params(TLNManagers)
  val tlNClients = params(TLNClients)
  val tlNCachingClients = params(TLNCachingClients)
  val tlNCachelessClients = params(TLNCachelessClients)
  val tlClientIdBits =  log2Up(tlNClients)
  val tlManagerIdBits =  log2Up(tlNManagers)
  val tlMaxClientXacts = params(TLMaxClientXacts)
  val tlMaxClientsPerPort = params(TLMaxClientsPerPort)
  val tlMaxManagerXacts = params(TLMaxManagerXacts)
  val tlClientXactIdBits = log2Up(tlMaxClientXacts*tlMaxClientsPerPort)
  val tlManagerXactIdBits = log2Up(tlMaxManagerXacts)
  val tlBlockAddrBits = params(TLBlockAddrBits)
  val tlDataBits = params(TLDataBits)
  val tlDataBytes = tlDataBits/8
  val tlDataBeats = params(TLDataBeats)
  val tlWriteMaskBits = if(tlDataBits/8 < 1) 1 else tlDataBits/8
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
  val tlNetworkPreservesPointToPointOrdering = params(TLNetworkIsOrderedP2P)
  val tlNetworkDoesNotInterleaveBeats = true
  val amoAluOperandBits = params(XLen)

  def opSizeToXSize(s: UInt) = MuxLookup(s, UInt("b111"), Array(
    MT_B  -> UInt(0),
    MT_H  -> UInt(1),
    MT_W  -> UInt(2),
    MT_D  -> UInt(3),
    MT_BU -> UInt(0),
    MT_HU -> UInt(1),
    MT_WU -> UInt(2)))
}

abstract class TLBundle extends Bundle with TileLinkParameters
abstract class TLModule extends Module with TileLinkParameters

/** Base trait for all TileLink channels */
trait TileLinkChannel extends TLBundle {
  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ClientToManagerChannel extends TileLinkChannel
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ManagerToClientChannel extends TileLinkChannel
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
trait ClientToClientChannel extends TileLinkChannel // Unused for now

/** Common signals that are used in multiple channels.
  * These traits are useful for type parameterizing bundle wiring functions.
  */

/** Address of a cache block. */
trait HasCacheBlockAddress extends TLBundle {
  val addr_block = UInt(width = tlBlockAddrBits)

  def conflicts(that: HasCacheBlockAddress) = this.addr_block === that.addr_block
  def conflicts(addr: UInt) = this.addr_block === addr
}

/** Sub-block address or beat id of multi-beat data */
trait HasTileLinkBeatId extends TLBundle {
  val addr_beat = UInt(width = tlBeatAddrBits)
}

/* Client-side transaction id. Usually Miss Status Handling Register File index */
trait HasClientTransactionId extends TLBundle {
  val client_xact_id = Bits(width = tlClientXactIdBits)
}

/** Manager-side transaction id. Usually Transaction Status Handling Register File index. */
trait HasManagerTransactionId extends TLBundle {
  val manager_xact_id = Bits(width = tlManagerXactIdBits)
}

/** A single beat of cache block data */
trait HasTileLinkData extends HasTileLinkBeatId {
  val data = UInt(width = tlDataBits)

  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}

/** The id of a client source or destination. Used in managers. */
trait HasClientId extends TLBundle {
  val client_id = UInt(width = tlClientIdBits)
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
class Acquire extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  // Actual bundle fields:
  val is_builtin_type = Bool()
  val a_type = UInt(width = tlAcquireTypeBits)
  val union = Bits(width = tlAcquireUnionBits)

  // Utility funcs for accessing subblock union:
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
  private def amo_offset(dummy: Int = 0) = addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBits/8))
  /** Bit offset of [[uncore.PutAtomic]] operand */
  def amo_shift_bits(dummy: Int = 0) = UInt(amoAluOperandBits)*amo_offset()
  /** Write mask for [[uncore.Put]], [[uncore.PutBlock]], [[uncore.PutAtomic]] */
  def wmask(dummy: Int = 0) = 
    Mux(isBuiltInType(Acquire.putAtomicType), 
      FillInterleaved(amoAluOperandBits/8, UIntToOH(amo_offset())),
      Mux(isBuiltInType(Acquire.putBlockType) || isBuiltInType(Acquire.putType),
        union(tlWriteMaskBits, 1),
        UInt(0, width = tlWriteMaskBits)))
  /** Full, beat-sized writemask */
  def full_wmask(dummy: Int = 0) = FillInterleaved(8, wmask())
  /** Complete physical address for block, beat or operand */
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())

  // Other helper functions:
  /** Message type equality */
  def is(t: UInt) = a_type === t //TODO: make this more opaque; def ===?

  /** Is this message a built-in or custom type */
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  /** Is this message a particular built-in type */
  def isBuiltInType(t: UInt): Bool = is_builtin_type && a_type === t 

  /** Does this message refer to subblock operands using info in the Acquire.union subbundle */ 
  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  /** Is this message a built-in prefetch message */
  def isPrefetch(dummy: Int = 0): Bool = isBuiltInType() && is(Acquire.prefetchType) 

  /** Does this message contain data? Assumes that no custom message types have data. */
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  /** Does this message contain multiple beats of data? Assumes that no custom message types have data. */
  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  /** Does this message require the manager to probe the client the very client that sent it?
    * Needed if multiple caches are attached to the same port.
    */
  def requiresSelfProbe(dummy: Int = 0) = Bool(false)

  /** Mapping between each built-in Acquire type (defined in companion object)
    * and a built-in Grant type.
    */
  def getBuiltInGrantType(dummy: Int = 0): UInt = {
    MuxLookup(this.a_type, Grant.putAckType, Array(
      Acquire.getType       -> Grant.getDataBeatType,
      Acquire.getBlockType  -> Grant.getDataBlockType,
      Acquire.putType       -> Grant.putAckType,
      Acquire.putBlockType  -> Grant.putAckType,
      Acquire.putAtomicType -> Grant.getDataBeatType,
      Acquire.prefetchType  -> Grant.prefetchAckType))
  }
}

/** [[uncore.Acquire]] with an extra field stating its source id */
class AcquireFromSrc extends Acquire with HasClientId

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
  def getType       = UInt("b000") // Get a single beat of data
  def getBlockType  = UInt("b001") // Get a whole block of data
  def putType       = UInt("b010") // Put a single beat of data
  def putBlockType  = UInt("b011") // Put a whole block of data
  def putAtomicType = UInt("b100") // Perform an atomic memory op
  def prefetchType  = UInt("b101") // Prefetch a whole block of data
  def typesWithData = Vec(putType, putBlockType, putAtomicType)
  def typesWithMultibeatData = Vec(putBlockType)
  def typesOnSubBlocks = Vec(putType, getType, putAtomicType)

  def fullWriteMask = SInt(-1, width = new Acquire().tlWriteMaskBits).toUInt

  // Most generic constructor
  def apply(
      is_builtin_type: Bool,
      a_type: Bits,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0),
      union: UInt = UInt(0)): Acquire = {
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
    val acq = Wire(new Acquire)
    acq := a
    acq
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
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      union = Cat(MT_Q, M_XRD, alloc))
  }
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      addr_byte: UInt,
      operand_size: UInt,
      alloc: Bool): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      union = Cat(addr_byte, operand_size, M_XRD, alloc))
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
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.getBlockType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block,
      union = Cat(MT_Q, M_XRD, alloc))
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
      addr_block: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.prefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = UInt(0),
      union = Cat(MT_Q, M_XRD, Bool(true)))
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
  */
object Put {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      wmask: UInt = Acquire.fullWriteMask): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putType,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      union = Cat(wmask, Bool(true)))
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
      wmask: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      union = Cat(wmask, (wmask != Acquire.fullWriteMask)))
  }
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      union = Cat(Acquire.fullWriteMask, alloc))
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
      addr_block: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.prefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = UInt(0),
      union = Cat(M_XWR, Bool(true)))
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
      data: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putAtomicType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block, 
      addr_beat = addr_beat, 
      data = data,
      union = Cat(addr_byte, operand_size, atomic_opcode, Bool(true)))
  }
}

/** The Probe channel is used to force clients to release data or cede permissions
  * on a cache block. Clients respond to Probes with [[uncore.Release]] messages.
  * The available types of Probes are customized by a particular
  * [[uncore.CoherencePolicy]].
  */
class Probe extends ManagerToClientChannel 
    with HasCacheBlockAddress {
  val p_type = UInt(width = tlCoh.probeTypeWidth)

  def is(t: UInt) = p_type === t
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

/** [[uncore.Probe]] with an extra field stating its destination id */
class ProbeToDst extends Probe with HasClientId

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
  def apply(p_type: UInt, addr_block: UInt): Probe = {
    val prb = Wire(new Probe)
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
  def apply(dst: UInt, p_type: UInt, addr_block: UInt): ProbeToDst = {
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
class Release extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val r_type = UInt(width = tlCoh.releaseTypeWidth)
  val voluntary = Bool()

  // Helper funcs
  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = tlCoh.releaseTypesWithData.contains(r_type)
  //TODO: Assumes all releases write back full cache blocks:
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1) && tlCoh.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !Bool(tlNetworkPreservesPointToPointOrdering)
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, UInt(0, width = tlByteAddrBits))
}

/** [[uncore.Release]] with an extra field stating its source id */
class ReleaseFromSrc extends Release with HasClientId

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
      data: UInt = UInt(0)): Release = {
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
class Grant extends ManagerToClientChannel 
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasManagerTransactionId {
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
  def makeFinish(dummy: Int = 0): Finish = {
    val f = Wire(Bundle(new Finish, { case TLMaxManagerXacts => tlMaxManagerXacts }))
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

/** [[uncore.Grant]] with an extra field stating its destination */
class GrantToDst extends Grant with HasClientId

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
      data: UInt): Grant = {
    val gnt = new Grant
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
      data: UInt = UInt(0)): GrantToDst = {
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
class Finish extends ClientToManagerChannel with HasManagerTransactionId {
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

/** Complete IO definition for incoherent TileLink, including networking headers */
class UncachedTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new Acquire))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val finish = new DecoupledIO(new LogicalNetworkIO(new Finish))
}

/** Complete IO definition for coherent TileLink, including networking headers */
class TileLinkIO extends UncachedTileLinkIO {
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
class ClientUncachedTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new Acquire)
  val grant     = new DecoupledIO(new Grant).flip
}

/** This version of TileLinkIO does not contain network headers. 
  * It is intended for use within client agents.
  */
class ClientTileLinkIO extends ClientUncachedTileLinkIO {
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
class ManagerTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new AcquireFromSrc).flip
  val grant     = new DecoupledIO(new GrantToDst)
  val finish    = new DecoupledIO(new Finish).flip
  val probe     = new DecoupledIO(new ProbeToDst)
  val release   = new DecoupledIO(new ReleaseFromSrc).flip
}

/** Utilities for safely wrapping a *UncachedTileLink by pinning probe.ready and release.valid low */
object TileLinkIOWrapper {
  def apply(utl: ClientUncachedTileLinkIO, p: Parameters): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)(p)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(utl: ClientUncachedTileLinkIO): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(tl: ClientTileLinkIO): ClientTileLinkIO = tl
  def apply(utl: UncachedTileLinkIO, p: Parameters): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)(p)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(utl: UncachedTileLinkIO): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(tl: TileLinkIO): TileLinkIO = tl
}

class TileLinkIOWrapper extends TLModule {
  val io = new Bundle {
    val in = new UncachedTileLinkIO().flip
    val out = new TileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.finish <> io.in.finish
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

class ClientTileLinkIOWrapper extends TLModule {
  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientTileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

/** Used to track metadata for transactions where multiple secondary misses have been merged
  * and handled by a single transaction tracker.
  */
class SecondaryMissInfo extends TLBundle // TODO: add a_type to merge e.g. Get+GetBlocks, and/or HasClientId
    with HasTileLinkBeatId
    with HasClientTransactionId

/** A helper module that automatically issues [[uncore.Finish]] messages in repsonse
  * to [[uncore.Grant]] that it receives from a manager and forwards to a client
  */
class FinishUnit(srcId: Int = 0, outstanding: Int = 2) extends TLModule with HasDataBeatCounters {
  val io = new Bundle {
    val grant = Decoupled(new LogicalNetworkIO(new Grant)).flip
    val refill = Decoupled(new Grant)
    val finish = Decoupled(new LogicalNetworkIO(new Finish))
    val ready = Bool(OUTPUT)
  }

  val g = io.grant.bits.payload

  if(tlNetworkPreservesPointToPointOrdering) {
    io.finish.valid := Bool(false)
    io.refill.valid := io.grant.valid
    io.refill.bits := g
    io.grant.ready := io.refill.ready
    io.ready := Bool(true)
  } else {
    // We only want to send Finishes after we have collected all beats of
    // a multibeat Grant. But Grants from multiple managers or transactions may
    // get interleaved, so we could need a counter for each.
    val done = if(tlNetworkDoesNotInterleaveBeats) {
      connectIncomingDataBeatCounterWithHeader(io.grant)
    } else {
      val entries = 1 << tlClientXactIdBits
      def getId(g: LogicalNetworkIO[Grant]) = g.payload.client_xact_id
      assert(getId(io.grant.bits) <= UInt(entries), "Not enough grant beat counters, only " + entries + " entries.")
      connectIncomingDataBeatCountersWithHeader(io.grant, entries, getId).reduce(_||_)
    }
    val q = Module(new FinishQueue(outstanding, io.grant.bits.header.src.clone))
    q.io.enq.valid := io.grant.fire() && g.requiresAck() && (!g.hasMultibeatData() || done)
    q.io.enq.bits.fin := g.makeFinish()
    q.io.enq.bits.dst := io.grant.bits.header.src

    io.finish.bits.header.src := UInt(srcId)
    io.finish.bits.header.dst := q.io.deq.bits.dst
    io.finish.bits.payload := q.io.deq.bits.fin
    io.finish.valid := q.io.deq.valid
    q.io.deq.ready := io.finish.ready

    io.refill.valid := io.grant.valid
    io.refill.bits := g
    io.grant.ready := (q.io.enq.ready || !g.requiresAck()) && io.refill.ready
    io.ready := q.io.enq.ready
  }
}

class FinishQueueEntry[T <: Data](dType: T) extends TLBundle {
  val fin = new Finish
  val dst = dType.clone
  override def clone = new FinishQueueEntry(dType).asInstanceOf[this.type]
}

class FinishQueue[T <: Data](entries: Int, dType: T) extends Queue(new FinishQueueEntry(dType), entries)

/** A port to convert [[uncore.ClientTileLinkIO]].flip into [[uncore.TileLinkIO]]
  *
  * Creates network headers for [[uncore.Acquire]] and [[uncore.Release]] messages,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Probe Probes]].
  * Responds to [[uncore.Grant]] by automatically issuing [[uncore.Finish]] to the granting managers.
  *
  * @param clientId network port id of this agent
  * @param addrConvert how a physical address maps to a destination manager port id
  */
class ClientTileLinkNetworkPort(clientId: Int, addrConvert: UInt => UInt) extends TLModule {
  val io = new Bundle {
    val client = new ClientTileLinkIO().flip
    val network = new TileLinkIO
  }

  val finisher = Module(new FinishUnit(clientId))
  finisher.io.grant <> io.network.grant
  io.network.finish <> finisher.io.finish

  val acq_with_header = ClientTileLinkHeaderCreator(io.client.acquire, clientId, addrConvert)
  val rel_with_header = ClientTileLinkHeaderCreator(io.client.release, clientId, addrConvert)
  val prb_without_header = DecoupledLogicalNetworkIOUnwrapper(io.network.probe)
  val gnt_without_header = finisher.io.refill

  io.network.acquire.bits := acq_with_header.bits
  io.network.acquire.valid := acq_with_header.valid && finisher.io.ready
  acq_with_header.ready := io.network.acquire.ready && finisher.io.ready
  io.network.release <> rel_with_header
  io.client.probe <> prb_without_header
  io.client.grant <> gnt_without_header
}

object ClientTileLinkHeaderCreator {
  def apply[T <: ClientToManagerChannel with HasCacheBlockAddress](
      in: DecoupledIO[T],
      clientId: Int,
      addrConvert: UInt => UInt): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := addrConvert(in.bits.addr_block)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** A port to convert [[uncore.ManagerTileLinkIO]].flip into [[uncore.TileLinkIO]].flip
  *
  * Creates network headers for [[uncore.Probe]] and [[uncore.Grant]] messagess,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Acquire]], [[uncore.Release]] and [[uncore.Finish]],
  * but supplies client_id instead.
  *
  * @param managerId the network port id of this agent
  * @param idConvert how a sharer id maps to a destination client port id
  */
class ManagerTileLinkNetworkPort(managerId: Int, idConvert: UInt => UInt) extends TLModule {
  val io = new Bundle {
    val manager = new ManagerTileLinkIO().flip
    val network = new TileLinkIO().flip
  }
  io.network.grant <> ManagerTileLinkHeaderCreator(io.manager.grant, managerId, (u: UInt) => u)
  io.network.probe <> ManagerTileLinkHeaderCreator(io.manager.probe, managerId, idConvert)
  io.manager.acquire.bits.client_id := io.network.acquire.bits.header.src
  io.manager.acquire <> DecoupledLogicalNetworkIOUnwrapper(io.network.acquire)
  io.manager.release.bits.client_id := io.network.release.bits.header.src
  io.manager.release <> DecoupledLogicalNetworkIOUnwrapper(io.network.release)
  io.manager.finish <> DecoupledLogicalNetworkIOUnwrapper(io.network.finish)
}

object ManagerTileLinkHeaderCreator {
  def apply[T <: ManagerToClientChannel with HasClientId](
      in: DecoupledIO[T],
      managerId: Int,
      idConvert: UInt => UInt): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(managerId)
    out.bits.header.dst := idConvert(in.bits.client_id)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** Struct for describing per-channel queue depths */
case class TileLinkDepths(acq: Int, prb: Int, rel: Int, gnt: Int, fin: Int)

/** Optionally enqueues each [[uncore.TileLinkChannel]] individually */
class TileLinkEnqueuer(depths: TileLinkDepths) extends Module {
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
  def apply(in: TileLinkIO, depths: TileLinkDepths)(p: Parameters): TileLinkIO = {
    val t = Module(new TileLinkEnqueuer(depths))(p)
    t.io.client <> in
    t.io.manager
  }
  def apply(in: TileLinkIO, depth: Int)(p: Parameters): TileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))(p)
  }
}

/** Utility functions for constructing TileLinkIO arbiters */
trait TileLinkArbiterLike extends TileLinkParameters {
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
    val arb = Module(new StableLockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
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
    val arb = Module(new StableLockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
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
    val arb = Module(new StableRRArbiter(mngr.bits, arbN))
    arb.io.in <> clts
    mngr <> arb.io.out
  }
}

/** Abstract base case for any Arbiters that have UncachedTileLinkIOs */
abstract class UncachedTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec.fill(arbN){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Abstract base case for any Arbiters that have cached TileLinkIOs */
abstract class TileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec.fill(arbN){new TileLinkIO}.flip
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
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends UncachedTileLinkIOArbiter(n) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int) extends UncachedTileLinkIOArbiter(n) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int) extends UncachedTileLinkIOArbiter(n) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends TileLinkIOArbiter(n) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int) extends TileLinkIOArbiter(n) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int) extends TileLinkIOArbiter(n) with UsesNewId

/** Concrete uncached client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientUncachedTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec.fill(arbN){new ClientUncachedTileLinkIO}.flip
    val out = new ClientUncachedTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Concrete client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec.fill(arbN){new ClientTileLinkIO}.flip
    val out = new ClientTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupClientSourceHeaderless(io.in.map(_.release), io.out.release)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Utility trait containing wiring functions to keep track of how many data beats have 
  * been sent or recieved over a particular [[uncore.TileLinkChannel]] or pair of channels. 
  *
  * Won't count message types that don't have data. 
  * Used in [[uncore.XactTracker]] and [[uncore.FinishUnit]].
  */
trait HasDataBeatCounters {
  type HasBeat = TileLinkChannel with HasTileLinkBeatId

  /** Returns the current count on this channel and when a message is done
    * @param inc increment the counter (usually .valid or .fire())
    * @param data the actual channel data
    * @param beat count to return for single-beat messages
    */
  def connectDataBeatCounter[S <: TileLinkChannel](inc: Bool, data: S, beat: UInt) = {
    val multi = data.hasMultibeatData()
    val (multi_cnt, multi_done) = Counter(inc && multi, data.tlDataBeats)
    val cnt = Mux(multi, multi_cnt, beat)
    val done = Mux(multi, multi_done, inc)
    (cnt, done)
  }

  /** Counter for beats on outgoing [[chisel.DecoupledIO]] */
  def connectOutgoingDataBeatCounter[T <: TileLinkChannel](in: DecoupledIO[T], beat: UInt = UInt(0)): (UInt, Bool) =
    connectDataBeatCounter(in.fire(), in.bits, beat)

  /** Returns done but not cnt. Use the addr_beat subbundle instead of cnt for beats on 
    * incoming channels in case of network reordering.
    */
  def connectIncomingDataBeatCounter[T <: TileLinkChannel](in: DecoupledIO[T]): Bool =
    connectDataBeatCounter(in.fire(), in.bits, UInt(0))._2

  /** Counter for beats on incoming DecoupledIO[LogicalNetworkIO[]]s returns done */
  def connectIncomingDataBeatCounterWithHeader[T <: TileLinkChannel](in: DecoupledIO[LogicalNetworkIO[T]]): Bool =
    connectDataBeatCounter(in.fire(), in.bits.payload, UInt(0))._2

  /** If the network might interleave beats from different messages, we need a Vec of counters,
    * one for every outstanding message id that might be interleaved.
    *
    * @param getId mapping from Message to counter id
    */
  def connectIncomingDataBeatCountersWithHeader[T <: TileLinkChannel with HasClientTransactionId](
      in: DecoupledIO[LogicalNetworkIO[T]],
      entries: Int,
      getId: LogicalNetworkIO[T] => UInt): Vec[Bool] = {
    Vec((0 until entries).map { i =>
      connectDataBeatCounter(in.fire() && getId(in.bits) === UInt(i), in.bits.payload, UInt(0))._2 
    })
  }

  /** Provides counters on two channels, as well a meta-counter that tracks how many
    * messages have been sent over the up channel but not yet responded to over the down channel
    *
    * @param max max number of outstanding ups with no down
    * @param up outgoing channel
    * @param down incoming channel
    * @param beat overrides cnts on single-beat messages
    * @param track whether up's message should be tracked
    * @return a tuple containing whether their are outstanding messages, up's count,
    *         up's done, down's count, down's done
    */
  def connectTwoWayBeatCounter[T <: TileLinkChannel, S <: TileLinkChannel](
      max: Int,
      up: DecoupledIO[T],
      down: DecoupledIO[S],
      beat: UInt = UInt(0),
      track: T => Bool = (t: T) => Bool(true)): (Bool, UInt, Bool, UInt, Bool) = {
    val cnt = Reg(init = UInt(0, width = log2Up(max+1)))
    val (up_idx, up_done) = connectDataBeatCounter(up.fire(), up.bits, beat)
    val (down_idx, down_done) = connectDataBeatCounter(down.fire(), down.bits, beat)
    val do_inc = up_done && track(up.bits)
    val do_dec = down_done
    cnt := Mux(do_dec,
            Mux(do_inc, cnt, cnt - UInt(1)),
            Mux(do_inc, cnt + UInt(1), cnt))
    (cnt > UInt(0), up_idx, up_done, down_idx, down_done)
  }
}

/** A super channel message container that can contain all kind of channels
  * Potential useful in a crossbar shared by all channels
  */
class SuperChannel extends TileLinkChannel
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
    val fin = new Finish
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
  def apply(acq: Acquire): SuperChannel = {
    val msg = new SuperChannel
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
  def apply(prb: Probe): SuperChannel = {
    val msg = new SuperChannel
    msg.ctype := probeType
    msg.mtype := prb.p_type
    msg.addr_block := prb.addr_block
    msg.hasMultibeatData := prb.hasMultibeatData()
    msg
  }

  // Release => SuperChannel
  def apply(rel: Release): SuperChannel = {
    val msg = new SuperChannel
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
  def apply(gnt: Grant): SuperChannel = {
    val msg = new SuperChannel
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
  def apply(fin: Finish): SuperChannel = {
    val msg = new SuperChannel
    msg.ctype := finishType
    msg.manager_xact_id := fin.manager_xact_id
    msg.hasMultibeatData := fin.hasMultibeatData()
    msg
  }

}

/** Super channel multiplexer to arbitrate all channels to
  * the shared super channel according to the defined priorities.
  */
class SuperChannelInputMultiplexer
    extends TLModule
{
  val io = new Bundle {
    val tl = new TileLinkIO().flip
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel))
  }


  def hasData(m: LogicalNetworkIO[SuperChannel]) = m.payload.hasMultibeatData()
  val arb = Module(new StableLockingArbiter(io.su.bits.clone, 3, tlDataBeats, Some(hasData _)))

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

class SuperChannelOutputMultiplexer
    extends TLModule
{
  val io = new Bundle {
    val tl = new TileLinkIO
    val su = Decoupled(new LogicalNetworkIO(new SuperChannel))
  }

  def hasData(m: LogicalNetworkIO[SuperChannel]) = m.payload.hasMultibeatData()
  val arb = Module(new StableLockingArbiter(io.su.bits.clone, 2, tlDataBeats, Some(hasData _)))

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
class SuperChannelInputDemultiplexer
    extends TLModule
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

class SuperChannelOutputDemultiplexer
    extends TLModule
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

class NASTIMasterIOTileLinkIOConverterHandler(id: Int) extends TLModule with NASTIParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTIMasterIO
    val rdy = Bool(OUTPUT)
    val tl_match = Bool(OUTPUT)
    val na_match = Bool(OUTPUT)
  }

  // liminations:
  val dataBits = tlDataBits*tlDataBeats 
  val dstIdBits = params(LNHeaderBits)
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(dstIdBits + tlClientXactIdBits < nastiXIdBits, "NASTIMasterIO converter is going truncate tags: " + dstIdBits + " + " + tlClientXactIdBits + " >= " + nastiXIdBits)
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

  // internal control registers
  val write_multiple_data = Reg(init=Bool(false))
  val read_multiple_data = Reg(init=Bool(false))
  val (nw_cnt, nw_finish) =
    Counter(io.nasti.w.fire() && write_multiple_data, tlDataBeats)
  val (nr_cnt, nr_finish) =
    Counter((io.nasti.r.fire() && read_multiple_data), tlDataBeats)
  val is_read = Reg(init=Bool(false))
  val is_write = Reg(init=Bool(false))
  val is_builtin = Reg(init=Bool(false))
  val tag_out = Reg(UInt(width = nastiXIdBits))
  val addr_out = Reg(UInt(width = nastiXAddrBits))
  val len_out = Reg(UInt(width = nastiXLenBits))
  val size_out = Reg(UInt(width = nastiXSizeBits))
  val g_type_out = Reg(UInt(width = tlGrantTypeBits))
  val cmd_sent = Reg(init=Bool(false))

  // signal to handler allocator
  io.rdy := !(is_read || is_write)
  io.tl_match := (tag_out === Cat(tl_acq.client_id, tl_acq.client_xact_id) ||
                   tag_out === Cat(tl_rel.client_id, tl_rel.client_xact_id)) && !io.rdy
  io.na_match := (na_b.id === tag_out || na_r.id === tag_out) && !io.rdy

  // assigning control registers
  when(io.nasti.b.fire()) {
    write_multiple_data := Bool(false)
    is_write := Bool(false)
    cmd_sent := Bool(false)
  }

  when(na_r.last && io.nasti.r.fire()) {
    read_multiple_data := Bool(false)
    is_read := Bool(false)
    cmd_sent := Bool(false)
  }

  when(io.tl.acquire.valid) {
    write_multiple_data := tl_acq.hasMultibeatData()
    read_multiple_data := !tl_acq.isBuiltInType() || tl_acq.isBuiltInType(Acquire.getBlockType)
    is_read := tl_acq.isBuiltInType() || !tl_acq.hasData()
    is_write := tl_acq.isBuiltInType() && tl_acq.hasData()
    is_builtin := tl_acq.isBuiltInType()
    tag_out := Cat(tl_acq.client_id, tl_acq.client_xact_id)
    addr_out := Mux(tl_acq.isBuiltInType(), tl_acq.full_addr(), tl_acq.addr_block << (tlBeatAddrBits + tlByteAddrBits))
    len_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType(), UInt(tlDataBeats-1), UInt(0))
    size_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType() || tl_acq.hasData(),
                    bytesToXSize(UInt(tlDataBytes)),
                    opSizeToXSize(tl_acq.op_size()))
    g_type_out := Mux(tl_acq.isBuiltInType(), tl_acq.getBuiltInGrantType(), UInt(0)) // assume MI or MEI
  }

  when(io.tl.release.valid) {
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
  io.nasti.w.valid := (io.tl.acquire.valid || io.tl.release.valid) && is_write
  na_w.strb := Mux(tl_acq.isSubBlockType(), tl_acq.wmask(), SInt(-1))
  na_w.data := Mux(io.tl.release.valid, tl_rel.data, tl_acq.data)
  na_w.last := Mux(io.tl.release.valid || tl_acq.hasMultibeatData(), nw_finish, io.tl.acquire.valid)

  // nasti.ar
  io.nasti.ar.valid := is_read && !cmd_sent
  io.nasti.ar.bits := io.nasti.aw.bits

  // nasti.b
  io.nasti.b.ready := io.tl.grant.fire()

  // nasti.r
  io.nasti.r.ready := io.tl.grant.fire()

  // tilelink acquire
  io.tl.acquire.ready := io.nasti.w.fire() || io.nasti.ar.fire()

  // tilelink release
  io.tl.release.ready := io.nasti.w.fire()

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

class NASTIMasterIOTileLinkIOConverter extends TLModule with NASTIParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTIMasterIO
  }

  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)
  io.tl.finish.ready := Bool(true)

  val handlerList = (0 until nastiHandlers).map(id => Module(new NASTIMasterIOTileLinkIOConverterHandler(id)))
  val tlMatches = Vec(handlerList.map(_.io.tl_match)).toBits
  val tlReady = Vec(handlerList.map(_.io.rdy)).toBits
  val tlHandlerId = Mux(tlMatches.orR,
                        PriorityEncoder(tlMatches),
                        PriorityEncoder(tlReady))
  val naMatches = Vec(handlerList.map(_.io.na_match)).toBits
  val naHandlerId = PriorityEncoder(naMatches)

  def doInternalOutputArbitration[T <: Data](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]]) {
    val arb = Module(new StableRRArbiter(out.bits, ins.size))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doInternalInputRouting[T <: Data](in: DecoupledIO[T], outs: Seq[DecoupledIO[T]], id: UInt) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && id === UInt(i) }
  }

  doInternalInputRouting(io.tl.acquire, handlerList.map(_.io.tl.acquire), tlHandlerId)
  val acq_rdy = Vec(handlerList.map(_.io.tl.acquire.ready))
  io.tl.acquire.ready := (tlMatches.orR || tlReady.orR) && acq_rdy(tlHandlerId)

  doInternalInputRouting(io.tl.release, handlerList.map(_.io.tl.release), tlHandlerId)
  val rel_rdy = Vec(handlerList.map(_.io.tl.release.ready))
  io.tl.release.ready := (tlMatches.orR || tlReady.orR) && rel_rdy(tlHandlerId)

  doInternalOutputArbitration(io.tl.grant, handlerList.map(_.io.tl.grant))

  doInternalOutputArbitration(io.nasti.ar, handlerList.map(_.io.nasti.ar))

  doInternalOutputArbitration(io.nasti.w, handlerList.map(_.io.nasti.w))

  doInternalOutputArbitration(io.nasti.aw, handlerList.map(_.io.nasti.aw))

  doInternalInputRouting(io.nasti.b, handlerList.map(_.io.nasti.b), naHandlerId)
  val na_b_rdy = Vec(handlerList.map(_.io.nasti.b.ready))
  io.nasti.b.ready := naMatches.orR && na_b_rdy(naHandlerId)

  doInternalInputRouting(io.nasti.r, handlerList.map(_.io.nasti.r), naHandlerId)
  val na_r_rdy = Vec(handlerList.map(_.io.nasti.r.ready))
  io.nasti.r.ready := naMatches.orR && na_r_rdy(naHandlerId)
}

class NASTILiteMasterIOTileLinkIOConverter extends TLModule with NASTIParameters with TileLinkParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTILiteMasterIO
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
  aw_fire := io.nasti.aw.fire()
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
  w_fire := io.nasti.w.fire()
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

