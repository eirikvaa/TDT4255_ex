
package Core
import chisel3._
import chisel3.core.Input
import chisel3.iotesters.PeekPokeTester
import chisel3.util.Counter
import utilz._

/**
  The Cyclic multiplier creates two Cyclic grids, one transposed, and multiplies them.
  */
class CyclicVecMat(matrixDims: Dims, dataWidth: Int) extends Module {

  val io = IO(
    new Bundle {

      val dataInA     = Input(UInt(dataWidth.W))
      val writeEnableA = Input(Bool())

      val dataInB     = Input(UInt(dataWidth.W))
      val writeEnableB = Input(Bool())

      val dataOut     = Output(UInt(dataWidth.W))
      val dataValid   = Output(Bool())
      val done        = Output(Bool())

    }
  )

  /**
    The dimensions are transposed because this is a vector * matrix multiplication

                [1, 2]
    [a, b, c] x [3, 4]
                [5, 6]

    Here the vector will output a, b, c, a, b, c, a...
    The Matrix is the type you made last exercise, so it is actually just 3 more vectors
    of length 2. In cycle 0 the values {1, 3, 5} may be selected, in cycle 1 {2, 4, 6}
    can be selected.

    However, you can make up for the impedance mismatch by transposing the matrix, storing
    the data in 2 vectors of length 3 instead.

    In memory matrixB will look like [1, 3, 5]
                                     [2, 4, 6]

    For a correct result, it is up to the user to input the data for matrixB in a transposed
    manner. This is done in the tests, you don't need to worry about it.
  */
  val dims = matrixDims.transposed

  // basic linAlg
  val lengthA = dims.cols

  val vecA                 = Module(new CyclicVector(lengthA, dataWidth)).io
  val matrixB              = Module(new CyclicVectorGrid(dims, dataWidth)).io
  val dotProductCalculator = Module(new CyclicDot(lengthA, dataWidth)).io
  val dataIsLoaded         = RegInit(Bool(), false.B)

  /**
    Your implementation here
    */
  // Create counters to keep track of when the matrix and vector has gotten all the data.
  // You can assume that writeEnable will be synchronized with the vectors. I.e for a vector
  // of length 3 writeEnable can only go from true to false and vice versa at T = 0, 3, 6, 9 etc
  val rowSelect = Counter(dims.rows)
  val currentCol = Counter(dims.cols)
  val vecADone = RegInit(Bool(), false.B)
  val matrixBDone = RegInit(Bool(), false.B)

  // Create counters to keep track of how far along the computation is.
  io.done := false.B
  io.dataOut := 0.U
  io.dataValid := false.B
  dotProductCalculator.dataInA := 0.U
  dotProductCalculator.dataInB := 0.U

  // Initialize stuff so the compiler don't complain.
  vecA.dataIn := 0.U
  matrixB.dataIn := 0.U

  // Set correct write enablel signals
  vecA.writeEnable := io.writeEnableA
  matrixB.writeEnable := io.writeEnableB

  // Set up the correct rowSelect for matrixB
  matrixB.rowSelect := rowSelect.value

  // Check if we're not done inputting data
  when (dataIsLoaded === false.B) {
	// Only input vector data if it's not done.
	when (vecADone === false.B) {
		vecA.dataIn := io.dataInA
	}

	// Only input matrix data if it's not done.
	when (matrixBDone === false.B) {
		matrixB.dataIn := io.dataInB
	}
  }

  // Data has been entered into the current index for both the vector and the
  // matrix, so we increment the current column.
  when (currentCol.inc()) {
	vecA.dataIn := io.dataInA
	matrixB.dataIn := io.dataInB
	vecADone := true.B

	// We finished inputting the vector data, so don't do this anymore.
	// Must reset the column index because we still have matrix rows to count.
	currentCol.value := 0.U

	// We'll check here if we just finished the last row. If not, increment the
	// row count and start at it again.
	when (rowSelect.inc()) {
	  // The input vector is done, so now it's all inputting matrix numbers.
	  matrixBDone := true.B

	  // We're done with both vector and matrix input.
	  dataIsLoaded := true.B
	}
  }

  // Variables for the calculation
  val colIndex = Counter(dims.cols)
  val rowIndex = Counter(dims.rows)

  when (dataIsLoaded) {
	// Data is fully loaded, we must start calculating stuff now.
	matrixB.rowSelect := rowIndex.value
	dotProductCalculator.dataInA := vecA.dataOut
	dotProductCalculator.dataInB := matrixB.dataOut
	io.dataOut := dotProductCalculator.dataOut

	when (colIndex.inc()) {
	  // We done multiply vector with row n. Jump to row n + 1 in matrix.
	  io.dataValid := true.B

	  when (rowIndex.inc()) {
		// We done multiplying all the shit.
		io.done := true.B
	  }
	}
  }

  /**
    In the solution I used the following to keep track of state
    You can use these if you want to, or do it however you see fit.
    */
  // val currentCol = Counter(dims.cols)
  // val rowSel = Counter(dims.rows)
  // val aReady = RegInit(Bool(), false.B)
  // val bReady = RegInit(Bool(), false.B)
  // val isDone = RegInit(Bool(), false.B)
  // val (inputCounterB, counterBWrapped) = Counter(io.writeEnableB, (dims.elements) - 1)
  // val (numOutputted, numOutputtedWrapped) = Counter(dataValid, lengthA)
  // val (inputCounterA, counterAWrapped) = Counter(io.writeEnableA, lengthA - 1)
}
