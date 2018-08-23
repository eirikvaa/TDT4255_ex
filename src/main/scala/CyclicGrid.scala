package Core
import chisel3._
import chisel3.core.Input
import chisel3.iotesters.PeekPokeTester
import utilz._

/**
  CyclicGrids hold n CyclicVecs. Unlike the CyclicVecs, CyclicGrids have a select signal for selecting
  which CyclicVec to work on, but these CyclicVecs can not be controlled from the outside.
  */
class CyclicVectorGrid(dims: Dims, dataWidth: Int) extends Module{

  val io = IO(new Bundle {

    val writeEnable = Input(Bool())
    val dataIn     = Input(UInt(dataWidth.W))
    val rowSelect    = Input(UInt(8.W))

    val dataOut    = Output(UInt(dataWidth.W))
  })

  val rows = Array.fill(dims.rows){ Module(new CyclicVector(dims.cols, dataWidth)).io }

  /**
    Your implementation here
    */
  io.dataOut := 0.U

  for (i <- rows.indices) {
	// For the most part this should just pass the signals along to the
	// different vectors.
	rows(i).dataIn := 0.U
	rows(i).writeEnable := false.B

	// Send in the data to the appropriate vector if we should write.
	when (io.rowSelect === i.U) {
	  rows(i).dataIn := io.dataIn
	  rows(i).writeEnable := io.writeEnable
	  io.dataOut := rows(i).dataOut
	}
  }
}
