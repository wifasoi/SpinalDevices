import spinal.core._
import spinal.lib._

import spinal.lib.graphic._
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import scala.util.Random
import spinal.lib._
import scala.math._
import spinal.core.internals._
import spinal.core.GenerationFlags._
import spinal.core.Formal._

import org.apache.commons.io._
import java.io._
import spinal.lib.eda.yosys._

class testCounter(start: Int,end: Int) extends Component{
    val io = new Bundle{
        val output = out UInt(log2Up(end) bits)
        val inc = in Bool
    }
    val dutCounter = Counter(start,end)
    io.output := dutCounter.value
    when(io.inc){
        dutCounter.increment()
    }

    GenerationFlags.formal{

        when(initstate()){
            assume(ClockDomain.current.isResetActive)
        }

        cover(dutCounter.willOverflow)
        assert(dutCounter.value <= end)
        //assert(dutCounter.value >= end)
        assert(dutCounter.valueNext <= end)
        assert(start <= dutCounter.value)
        assert(start <= dutCounter.valueNext)
        assert((dutCounter.value === dutCounter.valueNext -1) || dutCounter.willOverflow || !dutCounter.willIncrement)
        assert(!dutCounter.willOverflowIfInc || dutCounter.value === end )
        assert(!dutCounter.willOverflow || (dutCounter.value === end) || !dutCounter.willIncrement)
    }

}

object DutTests {
    def main(args: Array[String]): Unit = {
        val test = SpinalConfig(defaultConfigForClockDomains=ClockDomainConfig(resetActiveLevel=HIGH)).includeFormal.generateSystemVerilog(new testCounter(2,10))
        val dd = new YosysFlow(test,"tasty").formal(Mode.prove).use(Solver.boolector).step(100).run()
        val test2 = SpinalConfig().includeFormal.generateVerilog(new testCounter(2,10))
        val df = new YosysFlow(test2,"rrrr").nextpnr_ice40().setTarget(Ice40.hx8k,Ice40.pack.bg121).withPCF("test.pcf").targetFrequency(100 MHz).run
        //df.run()
        //target: String,pack: String, jsonPath : String = "", pcfPath : String
    }
  }
