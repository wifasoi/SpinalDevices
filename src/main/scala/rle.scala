// import spinal.core._
// import spinal.lib._

// import spinal.sim._
// import spinal.core._
// import spinal.core.sim._
// import scala.util.Random
// import scala.math._
// import spinal.core.internals._
// import spinal.core.GenerationFlags._
// import spinal.core.Formal._

// import org.apache.commons.io._
// import java.io._

// case class rleBus[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Bundle {
//   val data = dataType()
//   val lenght = Bits(depth bits)
// }

// case class rle[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Component{
//   val io = new Bundle{
//     val input = in(dataType())
//     val enable = in(Bool)
//     val output = master(Flow(rleBus(dataType,depth)))
//   }

//   io.output.valid := False

//   val input = RegNextWhen(io.input,io.enable) init(dataType().getZero)
//   val isNew = io.input =/= input
//   val count = Reg(UInt(depth bits)) init(0)
//   io.output.lenght := count.asBits
//   io.output.data := input

//   when(isNew && io.enable){
//     count := 0
//     io.output.valid := True && io.enable
//   } otherwise {
//     count := count + 1
//   }

//   /////////////////////////////
//   // FORMAL
//   /////////////////////////////
//   GenerationFlags.formal{
//    //assume(U(io.input) <= U(io.input).high)
//    /*past(io.input,io.output.lenght)*/
//    val clockcicle = Reg(UInt(32 bits)) init(0)
//    val clocknum = Reg(UInt(32 bits)) init(0)
//    val dataold = dataType()
//    dataold := dataold.getZero
//    when(io.input =/= io.input){
//      clocknum := clockcicle
//      assert(dataold =/= io.output.data && clocknum =/= U(io.output.lenght))
//      dataold := io.input
//      clockcicle := 0
//    }
//    // assert(io.output.valid)
//     //assert(io.output.asUInt >= pow(2,depth).toInt,"")

//   //  assert(changed(io.input) || isNew)
//   //     assert(!$initstate() && ($past(io_input,io_output_payload_lenght) == io_output_payload_data) && io_enable);
//     }

// }

// case class rld[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Component{
//   val io = new Bundle{
//     val input = slave(Stream(rleBus(dataType,depth)))
//     val output = out(dataType())
//     val enable = in(Bool)
//   }
//   val payload = RegNextWhen(io.input.payload,io.input.fire && io.enable)
//   payload.data init(dataType().getZero)
//   payload.lenght init(0)
//   val tickover = Bool
//   val counter = Reg(UInt(depth bits)) init(0)

//   io.output := payload.data
//   when(payload.lenght =/= counter.asBits && io.enable){
//     counter := counter + 1
//     io.input.ready := False
//   } otherwise {
//     counter := 0
//     io.input.ready := True
//   }

// }

// class toplevel[T <: BaseType](val dataType: HardType[T]) extends Component{
//   val io = new Bundle{
//     val input = in(dataType())
//     val output = out(dataType())
//     val enable = in(Bool)
//   }

//   //val fifo = StreamFifo(dataType(),100)
//   val re = new rle(dataType(),10)
//   val rd = new rld(dataType(),10)

//   rd.io.enable := io.enable
//   re.io.enable := io.enable
//   // re.io.input := io.input
//   // re.io.output >> rd.io.input
//   // io.output := rd.io.output

//   val dd = StreamFifo(rleBus(dataType,10),100)
//   dd.io.push.payload := re.io.output.payload
//   dd.io.push.valid := re.io.output.valid
//   //dd.io.push.ready := True

//   re.io.input := io.input
//   dd.io.pop >> rd.io.input
//   io.output := rd.io.output
// }

// object DutTests {
//   def main(args: Array[String]): Unit = {
//     SpinalConfig().includeFormal.generateSystemVerilog(new rle(UInt(8 bits),8))
//     SimConfig.withWave.compile(new toplevel(UInt(8 bits))).doSim{ dut =>
//     dut.io.enable #= false
//       dut.clockDomain.forkStimulus(period = 10)
//       dut.clockDomain.waitSampling(10)
//       dut.io.enable #= true

//       Suspendable.repeat(10){
//         dut.io.input #= Random.nextInt(256)
//         dut.clockDomain.waitSampling(Random.nextInt(100))
//       }
//       dut.io.enable #= false
//       dut.clockDomain.waitSampling(1000)
//     }
//   }
//   // def main(args: Array[String]): Unit = {
//   //   //SpinalConfig().includeFormal.generateSystemVerilog(new rle(8,10))
//   //   for(i <- 0 to 100) {
//   //     FileUtils.deleteDirectory(new File("simWorkspace"))
//   //     SimConfig.withWave.compile(new rle(Bits(8 bits), 10)).doSim { dut =>
//   //       dut.clockDomain.forkStimulus(period = 10)
//   //       dut.clockDomain.waitSampling(10)

//   //       Suspendable.repeat(10) {
//   //         dut.io.input #= Random.nextInt(256)
//   //         dut.clockDomain.waitSampling(Random.nextInt(100))
//   //       }
//   //     }
//   //     val vcd = new File(s"simWorkspace/rle${if(i > 0) "_" + i else ""}/test.vcd")
//   //     assert(vcd.exists())
//   //     assert(vcd.length() > 10000)
//   //   }
//   // }
// }
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

class rle[T <: BaseType](val dataType: HardType[T], val depth: Int) extends Component{
  val io = new Bundle{
    val input = in(dataType())
    val output = out(dataType())
    val lenght = out(Bits(depth bits))
    val valid = out(Bool)
  }

  io.valid := False

  val input = RegNext(io.input)
  val isNew = io.input =/= input
  val count = Reg(UInt(depth bits)) init(0)
  io.lenght := count.asBits
  io.output := input

  when(isNew){
    count := 0
    io.valid := True
  } otherwise {
    count := count + 1
  }

  /////////////////////////////
  // FORMAL
  /////////////////////////////
  //GenerationFlags.formal{
  //  assume(io.input.asUInt < pow(2,size).toInt)
  //  assert(io.output.asUInt >= pow(2,depth).toInt,"")

  //  assert(changed(io.input) || isNew)
  //  }

}

object DutTests {
  def main(args: Array[String]): Unit = {
    //SpinalConfig().includeFormal.generateSystemVerilog(new rle(8,10))
    SimConfig.withWave.compile(new rle(Bits(8 bits),10)).doSim{ dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(10)

      Suspendable.repeat(10){
        dut.io.input #= Random.nextInt(256)
        dut.clockDomain.waitSampling(Random.nextInt(100))
      }
    }
  }
}