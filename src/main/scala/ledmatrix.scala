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

// class matrixled() extends Componet{
//     val io = new Bundle{
//         val input = Rgb(RgbConfig(8,8,8))
//         val r = out Bool
//         val g = out Bool
//         val b = out Bool
//         val clk
//     }


// }

case class rgbBus() extends Bundle {
    val r = Bool
    val g = Bool
    val b = Bool

    def pseudoPWM(value: Rgb, counter: UInt) ={
        r := value.r < counter
        g := value.g < counter
        b := value.b < counter
    }
}

case class ledMatrixBus() extends Bundle {
    val rgb1 = rgbBus()
    //val rgb2 = rgbBus()
    val row = Bits(6 bits)
    val latch = Bool
    val oe = Bool // strobe
    val clk = Bool
}

// class linedriver(size: Int = 64) extends Component = {
//     val io = new Bundle{
//         val input = in Rgb(RgbConfig(8,8,8))

//     }
//     val lat : Bool = False
//     val framebuffer = mem(Rgb(RgbConfig(8,8,8)),size)

//     val counter = Reg(6 bits) init(0)//make variable
//     val pwmcounter = Reg(8 bits) init(0)

//     io.r := framebuffer.readSync(counter) < pwmcounter
//     io.g := framebuffer.readSync(counter) < pwmcounter
//     io.b := framebuffer.readSync(counter) < pwmcounter

//     when(counter === size){
//         lat := True
//     }
// }

class matrixLed(column: Int = 64, row: Int = 64) extends Component {
    val io = new Bundle{
        val output = out(ledMatrixBus())
        val input = slave(Stream(Rgb(RgbConfig(4,4,4))))
    }
    val framebuffer = Mem(Rgb(RgbConfig(4,4,4)),row*column)
    //write to the framebuffer
    val writeCounter = Counter(row*column)
    io.input.ready := False
    when(io.input.valid && !writeCounter.willOverflowIfInc){
        io.input.ready := True
        framebuffer(writeCounter)
        writeCounter.increment()
    }


    // val rowCounter = Reg(log2Up(row))
    // val columnCounter = Reg(log2Up(row))
    val rowCounter = Counter(column)
    val columnCounter = Counter(row)
    val colorCounter = Counter(16) //color resolution
    val frameDrawn = rowCounter.willOverflow &&  columnCounter.willOverflow && colorCounter.willOverflowIfInc

    //driving logic
    io.output.latch := False
    io.output.oe := False
    io.output.row := rowCounter.value.asBits
    when(rowCounter.willOverflow){
        columnCounter.increment()
        io.output.latch := True
        io.output.oe := True
    }
    when(rowCounter.willOverflow && columnCounter.willOverflow){
        colorCounter.increment()
    }

    when(writeCounter.willOverflowIfInc){
        rowCounter.increment()
    }
    
    //take value from framebuffer
    val addressPixel = UInt(log2Up(row*column) bits)
    addressPixel := rowCounter.value * columnCounter.value
    val colorPixel = framebuffer.readSync(address=addressPixel)

    //color pwm logic
    io.output.rgb1.pseudoPWM(colorPixel,colorCounter)

    //pass clock to display
    io.output.clk := ClockDomain.current.readClockWire


}

// object DutTests {
//     def main(args: Array[String]): Unit = {
//         SpinalConfig().generateVerilog(new matrixLed())
//         SimConfig.withWave.compile(new matrixLed()).doSim{ dut =>
//             dut.clockDomain.forkStimulus(period = 10)
//             dut.clockDomain.waitSampling(10)
//             SimTimeout(50000000)

//             for(repeat <- 0 until 10000){
//                 dut.io.input.r #= Random.nextInt(16)
//                 dut.io.input.g #= Random.nextInt(16)
//                 dut.io.input.b #= Random.nextInt(16)
//                 dut.io.input.valid #= true
//                 dut.clockDomain.waitActiveEdgeWhere(dut.io.input.ready.toBoolean)
//                 dut.io.input.valid #= false
//                 dut.clockDomain.waitActiveEdgeWhere(!dut.io.input.ready.toBoolean)
//             }
//         }
//     }
//   }
