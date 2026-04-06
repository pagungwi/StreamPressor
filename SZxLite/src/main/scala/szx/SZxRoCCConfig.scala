package chipyard

import org.chipsalliance.cde.config.{Config, Parameters}
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._

// Configuration to add SZx RoCC accelerator to a Chipyard design
/* Added parameters
      + parallelWidth (default = 8 elements): configurable width of parallel registers
      + cacheLineBytes (default Rocket Core = 64B): configurable length of L1D cache
      + burstWords (default = 16 x 4B words): # of words retrieved in each RoCC memory fetch
*/
class WithSZxRoCCAccelerator(parallelWidth: Int = 8, cacheLineBytes: Int = 64, burstWords: Int = 16 ) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => LazyModule(
      new szx.SZxRoCCAccelerator(OpcodeSet.custom0, parallelWidth, cacheLineBytes, burstWords)(p)
    )
  )
})

// Example configuration for a single-core Rocket with SZx accelerator
class SZxRocketConfig extends Config(
  new WithSZxRoCCAccelerator(parallelWidth=8, cacheLineBytes=64, burstWords=16) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)
class SZxRocketConfig16 extends Config(
  new WithSZxRoCCAccelerator(parallelWidth=16, cacheLineBytes=64, burstWords=16) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)

class SZxRocketConfig32 extends Config(
  new WithSZxRoCCAccelerator(parallelWidth=32, cacheLineBytes=64, burstWords=16) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)
