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
        assert(dutCounter.valueNext <= end)
        assert(dutCounter.value > start)
        //assert(dutCounter.valueNext > start)

    }
    
}

object DutTests {
    def main(args: Array[String]): Unit = {
        SpinalConfig().includeFormal.generateSystemVerilog(new testCounter(2,10))
    }
  }
