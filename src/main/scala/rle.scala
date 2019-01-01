import spinal.core._
import spinal.lib._

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import scala.util.Random
import scala.math._
import spinal.core.internals._
import spinal.core.GenerationFlags._
import spinal.core.Formal._

import org.apache.commons.io._
import java.io._

case class rleBus[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Bundle {
  val data = dataType()
  val lenght = Bits(depth bits)
}

case class rle[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Component{
  val io = new Bundle{
    val input = in(dataType())
    val enable = in(Bool)
    val output = master(Flow(rleBus(dataType,depth)))
  }

  io.output.valid := False

  val input = RegNextWhen(io.input,io.enable) init(dataType().getZero)
  val isNew = (io.input =/= input) && io.enable && !ClockDomain.current.isResetActive
  val count = Reg(UInt(depth bits)) init(0)
  io.output.lenght := count.asBits
  io.output.data := input

  when(isNew && io.enable){
    count := 0
    io.output.valid := True && io.enable
  } otherwise {
    count := count + 1
  }

  /////////////////////////////
  // FORMAL
  /////////////////////////////
  GenerationFlags.formal{
    // val last = dataType()
    // when( changed(input) ){
    //   last 
    //   assert(last != io.output)
    // }

    // when(initstate()){
    //   assume(ClockDomain.current.isResetActive)
    // } otherwise {
    //   assume(!ClockDomain.current.isResetActive)
    // }

    // when(ClockDomain.current.isResetActive){
    //   assert(! (io.output.data == dataType().getZero))
    //   assert(! (io.output.lenght == 0))
    // }

    // when(past(io.input =/= past(io.input)) && io.enable && !initstate() && past(ClockDomain.current.isResetActive)){
    //   assert(io.output.data =/= past(io.input))
    // }

    when(isNew && !ClockDomain.current.isResetActive){
      assert(count =/= 0)
    }

  }


}



case class rld[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(rleBus(dataType,depth)))
    val output = out(dataType())
    val enable = in(Bool)
  }
  val payload = RegNextWhen(io.input.payload,io.input.fire && io.enable)
  payload.data init(dataType().getZero)
  payload.lenght init(0)
  val tickover = Bool
  val counter = Reg(UInt(depth bits)) init(0)

  io.output := payload.data
  when(payload.lenght =/= counter.asBits && io.enable){
    counter := counter + 1
    io.input.ready := False
  } otherwise {
    counter := 0
    io.input.ready := True
  }

}

class toplevel[T <: BaseType](val dataType: HardType[T]) extends Component{
  val io = new Bundle{
    val input = in(dataType())
    val output = out(dataType())
    val enable = in(Bool)
  }

  //val fifo = StreamFifo(dataType(),100)
  val re = new rle(dataType(),10)
  val rd = new rld(dataType(),10)

  rd.io.enable := io.enable
  re.io.enable := io.enable
  // re.io.input := io.input
  // re.io.output >> rd.io.input
  // io.output := rd.io.output

  val dd = StreamFifo(rleBus(dataType,10),100)
  dd.io.push.payload := re.io.output.payload
  dd.io.push.valid := re.io.output.valid
  //dd.io.push.ready := True

  re.io.input := io.input
  dd.io.pop >> rd.io.input
  io.output := rd.io.output
}

// object DutTests {
//   def main(args: Array[String]): Unit = {
//     SpinalConfig().includeFormal.generateSystemVerilog(new rle(UInt(8 bits),8))
//     // SimConfig.withWave.compile(new toplevel(UInt(8 bits))).doSim{ dut =>
//     // dut.io.enable #= false
//     //   dut.clockDomain.forkStimulus(period = 10)
//     //   dut.clockDomain.waitSampling(10)
//     //   dut.io.enable #= true

//     //   (0 to 10).foreach{ x =>
//     //     dut.io.input #= Random.nextInt(256)
//     //     dut.clockDomain.waitSampling(Random.nextInt(100))
//     //   }
//     //   dut.io.enable #= false
//     //   dut.clockDomain.waitSampling(1000)
//     // }
//   }
// }
