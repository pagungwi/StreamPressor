package chipyard

import org.chipsalliance.cde.config.{Config, Parameters}
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._

// Configuration to add SZx RoCC accelerator to a Chipyard design
class WithSZxRoCCAccelerator(parallelWidth: Int = 8) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => LazyModule(
      new szx.SZxRoCCAccelerator(OpcodeSet.custom0)(p)
    )
  )
})

// Example configuration for a single-core Rocket with SZx accelerator
class SZxRocketConfig extends Config(
  new WithSZxRoCCAccelerator(8) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)
class SZxRocketConfig16 extends Config(
  new WithSZxRoCCAccelerator(16) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)

class SZxRocketConfig32 extends Config(
  new WithSZxRoCCAccelerator(32) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(64) ++
  new chipyard.config.AbstractConfig
)
