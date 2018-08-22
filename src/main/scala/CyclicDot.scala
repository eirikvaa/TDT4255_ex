package Core
import chisel3._
import chisel3.core.Input
import chisel3.util.Counter

class CyclicDot(elements: Int, dataWidth: Int) extends Module{

  val io = IO(new Bundle {
                val dataInA = Input(UInt(dataWidth.W))
                val dataInB = Input(UInt(dataWidth.W))

                val dataOut = Output(UInt(dataWidth.W))
                val outputValid = Output(Bool())
              })

  /**
    Keep track of how many elements have been accumulated. As the interface has no
    indicator that data can be invalid it should always be assumed that data IS valid.

    This in turn means that the counter should tick on every cycle
    */
  val counter = Counter(elements)
  val accumulator = RegInit(UInt(dataWidth.W), 0.U)

  /**
    Your implementation here
    */
  // Increment the value of the accumulator with the product of data in A and B
  // When the counter reaches elements set output valid to true and flush the accumulator
  io.outputValid := false.B
  io.dataOut := 0.U

  when (counter.value === (elements - 1).U) {
	io.dataOut := accumulator + (io.dataInA * io.dataInB)
	io.outputValid := true.B
	accumulator := 0.U
  }.otherwise {
	accumulator := accumulator + (io.dataInA * io.dataInB)
  }

  counter.inc()
}
