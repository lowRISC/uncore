// See LICENSE for license details.

package uncore
import Chisel._

//Memory Parameters
case object CacheBlockBytes extends Field[Int]
case object CacheBlockOffsetBits extends Field[Int]
case object PAddrBits extends Field[Int]
case object PgIdxBits extends Field[Int]
case object PgLevels extends Field[Int]
case object PgLevelBits extends Field[Int]
case object VPNBits extends Field[Int]
case object PPNBits extends Field[Int]
case object VAddrBits extends Field[Int]
case object ASIdBits extends Field[Int]
case object MIFTagBits extends Field[Int]
case object MIFDataBits extends Field[Int]

// IO space
case object IOBaseAddr0 extends Field[UInt]
case object IOAddrMask0 extends Field[UInt]
case object IOBaseAddr1 extends Field[UInt]
case object IOAddrMask1 extends Field[UInt]

//Params used by all caches
case object NSets extends Field[Int]
case object NWays extends Field[Int]
case object RowBits extends Field[Int]
case object NTLBEntries extends Field[Int]
case object CacheName extends Field[String]
/** Unique name per TileLink network*/
case object TLId extends Field[String]

// L1 I$
case object NBTBEntries extends Field[Int]
case object NRAS extends Field[Int]

// L1 D$
case object StoreDataQueueDepth extends Field[Int]
case object ReplayQueueDepth extends Field[Int]
case object NMSHRs extends Field[Int]
case object LRSCCycles extends Field[Int]
case object ECCCode extends Field[Option[Code]]
case object Replacer extends Field[() => ReplacementPolicy]

// L2 $
case object NAcquireTransactors extends Field[Int]
case object NSecondaryMisses extends Field[Int]
case object L2DirectoryRepresentation extends Field[DirectoryRepresentation]

// Tag $
case object TagBits extends Field[Int]
case object TCBlockBits extends Field[Int]
case object TCTransactors extends Field[Int]
case object TCBlockTags extends Field[Int]
case object TCBaseAddr extends Field[Int]

// Rocket Core Constants
case object XLen extends Field[Int]

// uncore parameters
case object NBanks extends Field[Int]
case object BankIdLSB extends Field[Int]
case object LNHeaderBits extends Field[Int]
/** Width of cache block addresses */
case object TLBlockAddrBits extends Field[Int]
/** Number of client agents */
case object TLNClients extends Field[Int]
/** Width of data beats */
case object TLDataBits extends Field[Int]
/** Number of data beats per cache block */
case object TLDataBeats extends Field[Int]
/** Whether the underlying physical network preserved point-to-point ordering of messages */
case object TLNetworkIsOrderedP2P extends Field[Boolean]
/** Number of manager agents */
case object TLNManagers extends Field[Int] 
/** Number of client agents that cache data and use custom [[uncore.Acquire]] types */
case object TLNCachingClients extends Field[Int]
/** Number of client agents that do not cache data and use built-in [[uncore.Acquire]] types */
case object TLNCachelessClients extends Field[Int]
/** Coherency policy used to define custom mesage types */
case object TLCoherencePolicy extends Field[CoherencePolicy]
/** Maximum number of unique outstanding transactions per manager */
case object TLMaxManagerXacts extends Field[Int]
/** Maximum number of unique outstanding transactions per client */
case object TLMaxClientXacts extends Field[Int]
/** Maximum number of clients multiplexed onto a single port */
case object TLMaxClientsPerPort extends Field[Int]

