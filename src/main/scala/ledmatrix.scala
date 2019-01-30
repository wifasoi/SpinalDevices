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
import spinal.lib.fsm._

import org.apache.commons.io._
import java.io._
import spinal.lib.eda.yosys._

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
        r := value.r > counter
        g := value.g > counter
        b := value.b > counter
    }
}

case class ledMatrixBus() extends Bundle {
    val rgb1 = rgbBus()
    //val rgb2 = rgbBus()
    val row = Bits(5 bits)
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

//RgbConfig(4,4,4)
// class lineDriver(colSize: Int = 64, rgbConf: RgbConfig, colorSpace: Int) extends Component {
//     val io = new Bundle{
//         val output = out(rgbBus())
//         val input = slave(Stream(Rgb(rgbConf)))
//     }

//     val lineBuffer = Mem(Rgb(rgbConf),colSize)
//     val memCount = Counter(0 to colSize-1)
//     val doRGB : Bool = memCount.willOverflowIfInc

//     val loadMemory = new Area{
//         io.input.ready := False
//         when(io.input.valid){
//             when(!doRGB){
//                 io.input.ready := True
//                 lineBuffer(memCount) := io.input.payload
//                 memCount.increment()
//             }
//         }
//     }

//     val driveLed = new Area{
//         val colorCount = Counter(0 to colorSpace)
//         val lineCounter = Counter(0 to colSize-1)
//         val color = lineBuffer.readSync(lineCounter)
//         io.output.r := False
//         io.output.g := False
//         io.output.b := False
//         when(doRGB && !lineCounter.willOverflowIfInc){
//             io.output.pseudoPWM(color,colorCount)
//             lineCounter.increment()
//         }
//         when(lineCounter.willOverflowIfInc){
//             colorCount.increment()
//             // memCount.clear()
//             lineCounter.clear()
//         }
//         when(colorCount.willOverflowIfInc){
//             memCount.clear()
//             lineCounter.clear()
//         }
//     }

//     GenerationFlags.formal{
//         when(initstate()){
//             assume(ClockDomain.current.readResetWire)
//         } otherwise {
//             assume(!ClockDomain.current.readResetWire)
//         }
//         assume(io.input.valid)
//         cover(doRGB)
//     }
// }

class lineDriver(colSize: Int = 64, rgbConf: RgbConfig, colorSpace: Int) extends Component {
    val io = new Bundle{
        val output = out(ledMatrixBus())
        val input = slave(Stream(Rgb(rgbConf)))
    }

    io.output.rgb1.r := False
    io.output.rgb1.g := False
    io.output.rgb1.b := False
    io.input.ready := False
    io.output.latch := False

    val fsm = new StateMachine{
        val lineBuffer = Mem(Rgb(rgbConf),colSize)
        val memCount = Counter(colSize)
        val colorCount = Counter(colorSpace)
        val lineCounter = Counter(colSize)
        val color = lineBuffer.readSync(lineCounter)


        val load : State = new State with EntryPoint{
            onEntry{
                io.input.ready := True
                memCount.clear()
            }
            whenIsActive{
                io.input.ready := True
                when(io.input.valid) {
                    memCount.increment()
                    lineBuffer(memCount) := io.input.payload
                    when(memCount.willOverflowIfInc){
                        goto(writeRGB)
                    }
                } otherwise {
                    goto(load)
                }
            }
            onExit{io.input.ready := False}
        }

        val writeRGB : State = new State{
            onEntry{
                colorCount.clear()
                lineCounter.clear()}
            whenIsActive{
                when(colorCount.willOverflowIfInc){
                    when(lineCounter.willOverflowIfInc){
                        goto(load)
                    } otherwise {
                        lineCounter.increment()
                        colorCount.clear()
                        goto(writeRGB)
                    }
                } otherwise {
                    io.output.rgb1.pseudoPWM(color,colorCount)
                    colorCount.increment()
                    goto(writeRGB)
                }
            }
            onExit{io.output.latch := True}
        }
    }

    GenerationFlags.formal{
        // io.input.r.addAttribute("anyval")
        // io.input.g.addAttribute("anyval")
        // io.input.b.addAttribute("anyval")
        when(initstate()){
            assume(ClockDomain.current.readResetWire)
        } otherwise {
            assume(!ClockDomain.current.readResetWire)
        }
        //assume(!initstate() && thing)
        cover(fsm.isActive(fsm.writeRGB))
        assume(io.input.valid)
        assume(io.input.r === 3)
    }
}

// class counter extends Component {
//     val io = new Bundle{
//         val outclk = out Bool
//         val refclock = out Bool

//     }

//     val d = Counter(256)
//     io.outclk := d.willOverflowIfInc
//     d.increment()
//     io.refclock := ClockDomain.current.readClockWire
// }

class testLine(colSize: Int = 64, rgbConf: RgbConfig, colorSpace: Int) extends Component{
    val io = new Bundle{
        val output = out(ledMatrixBus())
        val r = in Bool
        val g = in Bool
        val b = in Bool
    }
    val s = new lineDriver(colSize,rgbConf,colorSpace)
    io.output <> s.io.output
    s.io.input.valid := True
    s.io.input.r := io.r? U(2) | U(0)
    s.io.input.g := io.g? U(2) | U(0)
    s.io.input.b := io.b? U(2) | U(0)
    io.output.oe.removeAssignments()
    io.output.row.removeAssignments()
    io.output.clk.removeAssignments()
    io.output.row := 0

    io.output.oe := True
    io.output.clk := ClockDomain.current.readClockWire
}

object LedMatrix {
    def main(args: Array[String]): Unit = {
        def rtl = {
            // val s = new lineDriver(16,RgbConfig(2,2,2),4)
            // s.io.input.valid := True
            // s.io.input.g := 2
            // s.io.output.oe := True
            // s
            new testLine(16,RgbConfig(2,2,2),4)
        }
        val lineDriverConfig = SpinalConfig(defaultConfigForClockDomains=ClockDomainConfig(resetActiveLevel=HIGH))
        // val lineDriverConfig = SpinalConfig()
        val formal = lineDriverConfig.includeFormal.generateSystemVerilog(rtl)
        val synt = lineDriverConfig.generateVerilog(rtl)//.withoutEnumString()


        //val dd = new YosysFlow(formal,"lineDriver").formal(Mode.cover,step=300).use(Solver.yices).copy(append=500).run

        val df = new YosysFlow(synt,"lineDriver").nextpnr_ice40().setTarget(Ice40.up5k).withPCF("../ledPannel.pcf").targetFrequency(50 MHz).run

        Icepack(asc="testLine.asc",bin="testLine.bin",workDir="lineDriver").run
        Iceprog(bin="testLine.bin",workDir="lineDriver").run
        //val dg = iceprog(bin = "lineDriver.bin").run
    }
  }