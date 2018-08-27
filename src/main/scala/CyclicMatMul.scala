package Core
import chisel3._
import chisel3.core.Input
import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import utilz._

/**
  The Cyclic multiplier creates two Cyclic grids, one transposed, and multiplies them.
  */
class CyclicMultiplier(dims: Dims, dataWidth: Int) extends Module {

  val io = IO(new Bundle {

    val dataInA     = Input(UInt(dataWidth.W))
    val writeEnableA = Input(Bool())

    val dataInB     = Input(UInt(dataWidth.W))
    val writeEnableB = Input(Bool())

    val dataOut     = Output(UInt(dataWidth.W))
    val dataValid   = Output(Bool())
    val done        = Output(Bool())
  })


  /**
    Your implementation here
    */
  val rowCounter       = RegInit(UInt(8.W), 0.U)
  val colCounter       = RegInit(UInt(8.W), 0.U)

  val rowOutputCounter = RegInit(UInt(8.W), 0.U)

  val calculating      = RegInit(Bool(), false.B)
  val accumulator      = RegInit(UInt(8.W), 0.U)

  val resultReady      = RegInit(Bool(), false.B)


  /**
    Following the same principle behind the the vector matrix multiplication, by
    NOT transposing the dimensions.

    When writing a multiplier for a 3x2 matrix it's implicit that this means a
    3x2 matrix and 2x3, returning a 2x2 matrix. By not transposing the dimensions
    we get the same effect as in VecMat
    */
  val matrixA = Module(new CyclicVectorGrid(dims, dataWidth)).io
  val matrixB = Module(new CyclicVectorGrid(dims, dataWidth)).io

  matrixA.rowSelect := 0.U
  matrixA.dataIn := 0.U
  matrixA.writeEnable := false.B

  matrixB.rowSelect := 0.U
  matrixB.dataIn := 0.U
  matrixB.writeEnable := false.B

  val rowSelectA = Counter(dims.rows)
  val rowSelectB = Counter(dims.rows)
  val currentColA = Counter(dims.cols)
  val currentColB = Counter(dims.cols)
  val matrixADone = RegInit(Bool(), false.B)
  val matrixBDone = RegInit(Bool(), false.B)
  val dataIsLoaded = RegInit(Bool(), false.B)
  val dotProductCalculator = Module(new CyclicDot(dims.cols, dataWidth)).io

  // Pleasing the compiler god
  io.done := false.B
  io.dataOut := 0.U
  io.dataValid := false.B

  dotProductCalculator.dataInA := 0.U
  dotProductCalculator.dataInB := 0.U

  // Set correct write enable signals
  matrixA.writeEnable := io.writeEnableA
  matrixB.writeEnable := io.writeEnableB

  // Select initially correct rows
  matrixA.rowSelect := rowSelectA.value
  matrixB.rowSelect := rowSelectB.value

  when (dataIsLoaded === false.B) {

	when (matrixADone === false.B) {
	  matrixA.dataIn := io.dataInA
	}

	when (matrixBDone === false.B) {
	  matrixB.dataIn := io.dataInB
	}

	when (currentColA.inc() && currentColB.inc()) {
	  // Finished with row in matrix A and B.
	  currentColA.value := 0.U
	  currentColB.value := 0.U

	  when (rowSelectA.inc() && rowSelectB.inc()) {
		// Both matrices have been populated.
		dataIsLoaded := true.B
		matrixADone := true.B
		matrixBDone := true.B

		// Reset timers for reuse when calculating
		rowSelectA.value := 0.U
		rowSelectB.value := 0.U
		currentColA.value := 0.U
		currentColB.value := 0.U
	  }
	}
  }

  when (dataIsLoaded) {
	matrixA.rowSelect := rowSelectA.value
	matrixB.rowSelect := rowSelectB.value
	dotProductCalculator.dataInA := matrixA.dataOut
	dotProductCalculator.dataInB := matrixB.dataOut
	accumulator := accumulator + dotProductCalculator.dataOut

	when (currentColA.inc()) {
	  currentColB.inc()
	  currentColA.value := 0.U
	  rowSelectB.value := 0.U

	  when (rowSelectB.inc()) {
		when (rowSelectA.inc()) {
		  io.done := true.B
		}
	  }
	}.otherwise {
	  io.dataValid := true.B
	  io.dataOut := accumulator
	  accumulator := 0.U
	}
  }
}
