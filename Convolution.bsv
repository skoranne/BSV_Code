import BUtils::*;
import BRAM::*;
import Vector::*;
import FIFO::*;
import GetPut::*;
import GetPut_N::*;

// Type for the data. Using Bit#(data_width) for parameterization.
typedef Bit#(data_width) Data;

// A simple module for the MAC unit.
interface MAC;
    method Action put_operand1(Data a);
    method Action put_operand2(Data b);
    method ActionValue#(Data) get_result;
endinterface

(* synthesize *)
module mkMAC(MAC);
    Reg#(Data) a <- mkReg(0);
    Reg#(Data) b <- mkReg(0);
    Reg#(Data) result <- mkReg(0);
    Reg#(Bool) ready <- mkReg(False);

    rule do_mac;
        let product = a * b; // Simple multiply
        result <= product;
        ready <= True;
    endrule

    method Action put_operand1(Data val);
        a <= val;
    endmethod

    method Action put_operand2(Data val);
        b <= val;
    endmethod

    method ActionValue#(Data) get_result;
        ready <= False;
        return result;
    endmethod
endmodule

interface LineBufferIFC;
   method Action put(Data d) provisos(Bits#(Data,data_width));
   method ActionValue#(Data) get;   
endinterface


// A line buffer for efficient input data access, similar to NVDLA's CBUF.
(* synthesize *)
module mkLineBuffer#(
    Integer rows,
    Integer cols,
    Integer line_width,
    Integer data_width)(LineBufferIFC)
    provisos (
        Bits#(Data, data_width)
    );

    Vector#(line_width, FIFO#(Data)) fifos;
    for (Integer i = 0; i < line_width; i = i + 1) begin
        fifos[i] <- mkFIFO;
    end

    method Action put(Data in_data);
        for (Integer i = 0; i < line_width - 1; i = i + 1) begin
            let val <- fifos[i].first();
            fifos[i+1].enq(val);
        end
        fifos[0].enq(in_data);
    endmethod

    method ActionValue#(Data) get;
        let out_data <- fifos[line_width-1].first();
        for (Integer i = 0; i < line_width - 1; i = i + 1) begin
            fifos[i].deq();
        end
        fifos[line_width-1].deq();
        return out_data;
    endmethod

endmodule

interface ConvolutionIFC;
//    interface Put#(Vector#(1, Data)) put_input_data, // Streaming input data
//    interface Put#(Vector#(kernel_size * kernel_size, Data)) put_weights, // Weights
//    interface Get#(Data) get_output_data // Output
endinterface
// Main convolution module
(* synthesize *)
module mkConvolution#(
    Integer input_rows,
    Integer input_cols,
    Integer kernel_size,
    Integer padding,
    Integer stride,
    Integer data_width)( ConvolutionIFC )
    provisos (
        Bits#(Data, data_width)
    );

    // Assertions for valid configuration
    `ifndef BSV_ASSERT_DISABLED
    let check_params = $assert(padding == 0 || padding == 1, "Padding must be 0 or 1");
    let check_kernel = $assert(kernel_size == 3, "Kernel size must be 3 for this example");
    `endif

    // The output dimensions depend on padding and stride.
    Integer output_rows = (input_rows + 2*padding - kernel_size) / stride + 1;
    Integer output_cols = (input_cols + 2*padding - kernel_size) / stride + 1;

    // Line buffer to store a few rows of the input feature map.
    // This example assumes kernel_size=3.
    // To implement general convolution with various kernel sizes, this needs to be more complex.
    //LineBuffer#(kernel_size*input_cols, kernel_size, Data) line_buffer <- mkLineBuffer(kernel_size*input_cols, kernel_size, input_cols, data_width);
    LineBuffer line_buffer <- mkLineBuffer;

    // BRAM for weights, similar to NVDLA's convolution buffer.
    BRAM_1_port#(Bit#(TLog#(kernel_size*kernel_size)), Data) weights_ram <- mkBRAM_1_port(0);

    // FIFOs for streaming data in and out
    FIFO#(Vector#(1, Data)) input_fifo <- mkFIFO;
    FIFO#(Vector#(kernel_size*kernel_size, Data)) weights_fifo <- mkFIFO;
    FIFO#(Data) output_fifo <- mkFIFO;

    // Registers to control the convolution process
    Reg#(Integer) input_row_counter <- mkReg(0);
    Reg#(Integer) input_col_counter <- mkReg(0);
    Reg#(Integer) output_row_counter <- mkReg(0);
    Reg#(Integer) output_col_counter <- mkReg(0);
    Reg#(Integer) kernel_row_counter <- mkReg(0);
    Reg#(Integer) kernel_col_counter <- mkReg(0);
    Reg#(Data) accumulator <- mkReg(0);

    // State machine for the process
    Reg#(Bool) weights_loaded <- mkReg(False);

    // The actual MAC units for parallel computation
    Vector#(kernel_size*kernel_size, MAC) macs;
    for(Integer i=0; i<valueOf(kernel_size*kernel_size); i=i+1) begin
        macs[i] <- mkMAC;
    end

    // Rule to load weights from the input FIFO into the BRAM.
    // Assumes weights are streamed in first.
    rule load_weights (weights_loaded == False);
        let w_vec = weights_fifo.first;
        for (Integer i = 0; i < valueOf(kernel_size*kernel_size); i = i + 1) begin
            weights_ram.put(unpack(pack(i)), w_vec[i]);
        end
        weights_fifo.deq();
        weights_loaded <= True;
    endrule

    // Rule to put data into the line buffer.
    rule feed_line_buffer (weights_loaded == True && input_row_counter < input_rows);
        let in_data = input_fifo.first;
        input_fifo.deq();
        line_buffer.put(in_data[0]);
        input_col_counter <= input_col_counter + 1;
        if (input_col_counter == input_cols - 1) begin
            input_col_counter <= 0;
            input_row_counter <= input_row_counter + 1;
        end
    endrule

    // Rule to perform the convolution and accumulate results.
    rule convolve (weights_loaded == True && input_row_counter >= kernel_size-1);
        let kernel_row = kernel_row_counter;
        let kernel_col = kernel_col_counter;

        // Perform MAC operations
        for (Integer i = 0; i < valueOf(kernel_size*kernel_size); i = i + 1) begin
            let w_val = weights_ram.readRequest(unpack(pack(i)));
            // This is a simplified fetch; proper CBUF access is more complex.
            // Simplified line_buffer access for the example.
            let input_val = line_buffer.get; // Simplification, not true CBUF behavior
            macs[i].put_operand1(input_val);
            macs[i].put_operand2(w_val);
        end

        // Accumulate and store
        Data total_sum = 0;
        for (Integer i = 0; i < valueOf(kernel_size*kernel_size); i = i + 1) begin
            let result <- macs[i].get_result;
            total_sum = total_sum + result; // Accumulation
        end
        accumulator <= total_sum;

        // Update counters for the next step.
        kernel_col_counter <= kernel_col_counter + 1;
        if (kernel_col_counter == kernel_size - 1) begin
            kernel_col_counter <= 0;
            kernel_row_counter <= kernel_row_counter + 1;
        end

        // This simplified logic is for demonstration.
        // The real NVDLA scheduling is much more complex.
        // The output_fifo.enq will happen only after a full output pixel is computed.
        if (kernel_row_counter == kernel_size - 1 && kernel_col_counter == kernel_size - 1) begin
            output_fifo.enq(accumulator);
            accumulator <= 0;
            output_col_counter <= output_col_counter + 1;
            if (output_col_counter == output_cols - 1) begin
                output_col_counter <= 0;
                output_row_counter <= output_row_counter + 1;
            end
        end
    endrule

    // Interface methods
    method put_input_data = toPut(input_fifo);
    method put_weights = toPut(weights_fifo);
    method get_output_data = toGet(output_fifo);

endmodule