
package dummy_tile

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{DCacheLogicalTreeNode, LogicalModuleTree, RocketLogicalTreeNode, UTLBLogicalTreeNode}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.prci.{ClockSinkParameters}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.subsystem.CanAttachTile
import testchipip.TLHelper

case class DummyCoreParams(
  bootFreqHz: BigInt = 0,
  useVM: Boolean = false,
  useUser: Boolean = false,
  useSupervisor: Boolean = false,
  useHypervisor: Boolean = false,
  useDebug: Boolean = false,
  useAtomics: Boolean = false,
  useAtomicsOnlyForIO: Boolean = false,
  useCompressed: Boolean = false,
  useRVE: Boolean = false,
  useSCIE: Boolean = false,
  nLocalInterrupts: Int = 0,
  useNMI: Boolean = false,
  nBreakpoints: Int = 0,
  useBPWatch: Boolean = false,
  mcontextWidth: Int = 0,
  scontextWidth: Int = 0,
  nPMPs: Int = 0,
  nPerfCounters: Int = 0,
  haveBasicCounters: Boolean = false,
  haveCFlush: Boolean = false,
  misaWritable: Boolean = false,
  nL2TLBEntries: Int = 0,
  nL2TLBWays: Int = 1,
  nPTECacheEntries: Int = 8,
  mtvecInit: Option[BigInt] = None,
  mtvecWritable: Boolean = false,
  fastLoadWord: Boolean = false,
  fastLoadByte: Boolean = false,
  branchPredictionModeCSR: Boolean = false,
  clockGate: Boolean = false,
  mvendorid: Int = 0, // 0 means non-commercial implementation
  mimpid: Int = 0x20200312, // release date in BCD
  mulDiv: Option[MulDivParams] = None,
  fpu: Option[FPUParams] = None
) extends CoreParams {
  val lgPauseCycles = 5
  val haveFSDirty = false
  val pmpGranularity: Int = if (useHypervisor) 4096 else 4
  val fetchWidth: Int = if (useCompressed) 2 else 1
  //  fetchWidth doubled, but coreInstBytes halved, for RVC:
  val decodeWidth: Int = fetchWidth / (if (useCompressed) 2 else 1)
  val retireWidth: Int = 1
  val instBits: Int = if (useCompressed) 16 else 32
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
}

case class DummyTileParams(
  core : DummyCoreParams = DummyCoreParams(),
  icache : Option[ICacheParams] = Some(ICacheParams()),
  dcache : Option[DCacheParams] = Some(DCacheParams()),
  btb : Option[BTBParams] = None,
  dataScratchpadBytes : Int = 0,
  name : Option[String] = Some("dummy-tile"),
  hartId : Int = 0,
  beuAddr: Option[BigInt] = None,
  blockerCtrlAddr: Option[BigInt] = None,
  clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
  boundaryBuffers: Boolean = false
) extends InstantiableTileParams[DummyTile] {
    def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): DummyTile = {
    new DummyTile(this, crossing, lookup)
  }
}

case class DummyTileAttachParams(
  tileParams : DummyTileParams,
  crossingParams : RocketCrossingParams
) extends CanAttachTile {
  type TileType = DummyTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}

class DummyTile(
  val myParams : DummyTileParams,
  crossing : ClockCrossingType,
  lookup : LookupByHartIdImpl,
  q : Parameters)
    extends BaseTile(myParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasHellaCache
    with HasTileParameters
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: DummyTileParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  // Require TileLink nodes
  val intOutwardNode = IntIdentityNode()
  val masterNode = visibilityNode
  val slaveNode = TLIdentityNode()

  tlOtherMastersNode := tlMasterXbar.node
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  override lazy val module = new DummyTileImp(this)

  lazy val icache : ICache = LazyModule(new ICache(myParams.icache.get, myParams.hartId))
  tlMasterXbar.node := icache.masterNode
  icache.hartIdSinkNodeOpt.map { _ := hartIdNexusNode }
  icache.mmioAddressPrefixSinkNodeOpt.map { _ := mmioAddressPrefixNexusNode }

  nDCachePorts += 1

  // Required entry of CPU device in the device tree for interrupt purpose
  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("botbakery,dummy-cpu", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++
                        cpuProperties ++
                        nextLevelCacheProperty ++
                        tileProperties)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(myParams.hartId))
  }
}

class DummyTileImp(outer : DummyTile) extends BaseTileModuleImp(outer)
    with HasHellaCacheModule
{
  Annotated.params(this, outer.myParams)

  val dCacheIO = dontTouch(Wire(new HellaCacheIO))
  dCacheIO <> outer.dcache.module.io.cpu
  dCacheIO.req.valid := false.B
  dCacheIO.req.bits.addr := 0.U
  dCacheIO.req.bits.data := 0.U
  dCacheIO.req.bits.mask := 0.U
  dCacheIO.req.bits.tag := 0.U
  dCacheIO.req.bits.cmd := 0.U
  dCacheIO.req.bits.size := 0.U
  dCacheIO.req.bits.signed := false.B
  dCacheIO.req.bits.dprv := 0.U
  dCacheIO.req.bits.dv := false.B
  dCacheIO.req.bits.phys := false.B
  dCacheIO.req.bits.no_alloc := false.B
  dCacheIO.req.bits.no_xcpt := false.B
  dCacheIO.s1_kill := false.B
  dCacheIO.s1_data.data := 0.U
  dCacheIO.s1_data.mask := 0.U
  dCacheIO.s2_kill := false.B
  dCacheIO.keep_clock_enabled := false.B

  val iCacheIO = dontTouch(Wire(Decoupled(new ICacheReq)))
  iCacheIO <> outer.icache.module.io.req
  iCacheIO.valid := false.B
  iCacheIO.bits.addr := 0.U

  // Connect interrupts
  val debug_i = dontTouch(Wire(Bool()))
  val mtip_i = dontTouch(Wire(Bool()))
  val int_bundle = dontTouch(Wire(new TileInterrupts()))
  outer.decodeCoreInterrupts(int_bundle)
  debug_i := int_bundle.debug
  mtip_i := int_bundle.meip & int_bundle.msip & int_bundle.mtip
}

class WithNDummyCores(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    // Calculate the next available hart ID (since hart ID cannot be duplicated)
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    // Create TileAttachParams for every core to be instantiated
    (0 until n).map { i =>
      DummyTileAttachParams(
        tileParams = DummyTileParams(hartId = i + idOffset),
        crossingParams = RocketCrossingParams()
      )
    } ++ prev
  }
  // Configurate # of bytes in one memory / IO transaction. For RV64, one load/store instruction can transfer 8 bytes at most.
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
  // The # of instruction bits. Use maximum # of bits if your core supports both 32 and 64 bits.
  case XLen => 64
})
