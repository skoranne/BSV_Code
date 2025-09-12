// clz_rec is the recursive function that counts leading zeros
function Integer clz_rec(Bit#(n) x);
    if (valueOf(n) == 1) begin
        return (x == 1'b0) ? 1 : 0;
    end else begin
        let msb = x[valueOf(n)-1];
        let half_width = valueOf(n) / 2;
        if (msb == 1'b1) begin // If the MSB is 1, no leading zeros
            return 0;
        end else begin
            let upper_half = x[valueOf(n)-1 : half_width];
            let lower_half = x[half_width-1 : 0];

            if (upper_half == 1'b0) begin // Upper half is all zeros
                return half_width + clz_rec(lower_half);
            end else begin // First 1 is in the upper half
                return clz_rec(upper_half);
            end
        end
    end
endfunction

// The main function for counting leading zeros
function Integer clz(Bit#(n) x);
    if (x == 1'b0) begin
        return valueOf(n); // Special case for all zeros
    end else begin
        return clz_rec(x);
    end
endfunction
