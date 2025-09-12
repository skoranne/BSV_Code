package C2;

import FIFO::*;
import Vector::*;
import RegFile::*;
import BRAM::*;
import GetPut::*;
import Connectable::*;

// ========================================================================
// Type Definitions
// ========================================================================

// Parameterized data width for inputs, weights, and outputs.
typedef 8 TDataWidth;
typedef 16 TAccWidth;

typedef Bit#(TDataWidth) TData;
typedef Bit#(TAccWidth) TAcc;

// Kernel dimensions
Integer kernel_size = 3;
Integer kernel_channels = 3;
Integer num_kernels = 16;

// Image tile dimensions
Integer tile_width = 32;
Integer tile_height = 32;
Integer tile_channels = 3;

// Interface for the convolution module
interface ConvInterface;
    // Input for feature data
    method Action put_input_pixel(TData pixel);
    // Input for weight data
    method Action put_weight(TData weight);
    // Output for accumulated result
    method ActionValue#(TAcc) get_output_acc();
endinterface

// ========================================================================
// Module: mkConvolutionCore
// A tiled, pipelined 2D convolution module.
// ========================================================================

module mkConvolutionCoreImpl#(numeric cKERNEL_SIZE,
                              numeric cKERNEL_CHANNELS,
                              numeric cNUM_KERNELS,
                              numeric cTILE_WIDTH,
                              numeric cTILE_HEIGHT,
                              numeric cTILE_CHANNELS)(ConvInterface);
    // -- FIFOs for streaming data --
    FIFO#(TData) input_fifo <- mkSizedFIFO(cTILE_WIDTH * cTILE_HEIGHT * cTILE_CHANNELS);
    FIFO#(TData) weight_fifo <- mkSizedFIFO(cKERNEL_SIZE * cKERNEL_SIZE * cKERNEL_CHANNELS * cNUM_KERNELS);

    // -- Registers for control and accumulation --
    Reg#(Bit#(5)) tile_x <- mkReg(0);
    Reg#(Bit#(5)) tile_y <- mkReg(0);
    Reg#(Bit#(5)) tile_c <- mkReg(0);

    // Accumulators for storing partial sums
    RegFile#(Bit#(8), TAcc) accumulators <- mkRegFile(0, cNUM_KERNELS - 1);

    // Register to hold the current kernel data. This mimics NVDLA's CBUF
    // for a single atomic operation's weights.
    Reg#(Vector#(cNUM_KERNELS, Vector#(cKERNEL_SIZE, Vector#(cKERNEL_SIZE, Vector#(cKERNEL_CHANNELS, TData))))) 
        kernel_buffer <- mkReg(replicate(replicate(replicate(replicate(0)))));

    // FSM State Registers
    Reg#(Integer) state <- mkReg(0); // 0: Load Weights, 1: Process Tile

    // -- Rules --

    // Rule to load all weights for one convolution layer.
    // In a real NVDLA-like system, this would be handled by CDMA and a larger CBUF.
    rule rl_load_weights (state == 0 && weight_fifo.notEmpty);
        let weight_val = weight_fifo.first;
        weight_fifo.deq;

        // Load into a temporary vector for now
        // In a realistic design, this logic would be more complex and indexed.
        // For simplicity, we just check if fifo is empty after deq.
        if (weight_fifo.notEmpty) begin
            // Complex indexing logic would go here. For now, we assume
            // the weight_fifo loads sequentially into a buffer.
        end
        if (weight_fifo.notEmpty) begin
             state <= 1; // Transition to processing
        end
    endrule

    // Rule for the main convolution loop
    rule rl_process_convolution (state == 1 && input_fifo.notEmpty);
        // The core convolution logic will happen here.
        // This rule represents one MAC operation on one pixel.
        let input_pixel = input_fifo.first;
        input_fifo.deq;

        // This is a highly simplified representation. NVDLA's CMAC performs
        // many MACs in parallel. The Bluespec would use a systolic array
        // or a similar parallel structure.
        // For each output kernel:
        for (Integer k = 0; k < cNUM_KERNELS; k = k + 1) begin
            // Loop through the kernel dimensions (simplified for clarity)
            for (Integer r = 0; r < cKERNEL_SIZE; r = r + 1) begin
                for (Integer s = 0; s < cKERNEL_SIZE; s = s + 1) begin
                    for (Integer c = 0; c < cKERNEL_CHANNELS; c = c + 1) begin
                        // Simplified MAC operation
                        TAcc current_acc = accumulators.read(k);
                        // This uses a fixed weight from the kernel_buffer, which is oversimplified.
                        // A realistic approach involves data reuse and more complex indexing.
                        let weight = 1; // dummy value
                        let product = extend(input_pixel) * extend(weight);
                        accumulators.upd(k, current_acc + product);
                    end
                end
            end
        end

        // Output logic (very simplified)
        // In a real system, the accumulator values would be read out after
        // a tile is complete, and then the accumulator would be reset.
        // For this simplified example, we will not drain the accumulator.
    endrule

    // -- Methods --
    method Action put_input_pixel(TData pixel);
        input_fifo.enq(pixel);
    endmethod

    method Action put_weight(TData weight);
        weight_fifo.enq(weight);
    endmethod

    // The get method is a placeholder. A full design would manage accumulation
    // and output after each tile, not on a simple per-pixel basis.
    method ActionValue#(TAcc) get_output_acc();
        // This is a highly simplified output. A full NVDLA would have CACC
        // write results to memory.
        return accumulators.read(0);
    endmethod
endmodule

module mkConvolutionCore(ConvInterface);
  ConvInterface ifc <- mkConvolutionCoreImpl(3,3,8,12,16,16);
  return ifc;
endmodule
  

endpackage
