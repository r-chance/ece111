module sha256(
  input logic clk,
              reset_n,
              start,
  input logic [31:0] message_addr,
                     size,
                     output_addr,
  output logic done,
               mem_clk,
               mem_we,
  output logic [15:0] mem_addr,
  output logic [31:0] mem_write_data,
  input logic [31:0] mem_read_data
);
  
  parameter int K[0:63] = '{ 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

  // state variables
  bit waiting = 1;// set waiting to true by default
  bit gettingBlock = 0;
  bit processingBlock = 0;
  bit writing = 0;
  bit memAddrSet;
  bit valueReady;
  bit allProcessed;
  bit paddingDone;
  bit paddedFirst;
  bit wNotFinished;
  bit [7:0] t;// index variable
  
  bit [63:0] blockNumber;// indicates number of 512 bit block that is being processed


  logic [31:0] W[16];// storage for each w_t so they don't need to be calculated multiple times.

  bit lastBlockProcessed;// used to indicate that the last block has been fetched

  logic [511:0] lastBlock;

  bit [31:0] remaining;
  bit [63:0] size_64;

  logic [31:0] h[8];
  
  bit [5:0] i;
  bit[31:0] b;
  bit[31:0] numToClear;

  bit delimited = 0;
  bit [31:0] padding;

  // Temporary variables
  logic [31:0] A,B,C,D,E,F,G,H,
               s0,s1,
               maj,ch,
               t1,t2,
               S0,S1;

  // set memory clock
  assign mem_clk = clk;

  // sequential logic to be performed with the clock
  always_ff @(posedge clk) begin
    mem_we <= 0;


    //** START
    // Turn off waiting upon start flag
    if(start) begin
      waiting <= 0;
      h[0] <= 'h6a09e667;
      h[1] <= 'hbb67ae85;
      h[2] <= 'h3c6ef372;
      h[3] <= 'ha54ff53a;
      h[4] <= 'h510e527f;
      h[5] <= 'h9b05688c;
      h[6] <= 'h1f83d9ab;
      h[7] <= 'h5be0cd19;
      gettingBlock <= 1;
      mem_addr <= message_addr;// get first mem address
      remaining <= size*8;
      firstSize <= 0;// indicates size hasn't been written to W yet
      allProcessed <= 0;
      delimited <= 0;

      // Determine size of padding
      if(((size*8)%512)+65 <= 512) begin // This fits in last block cleanly

        padding <= 512 - (((size*8)%512)+65);
      end

      else begin // This fits in two blocks
        padding <= 448+(511-((size*8)%512));
      end
    end
 


    //** GETTINGBLOCK STATE
    else if(!waiting && gettingBlock && !processingBlock && !writing) begin

      // if values remain to be fetched
      if(remaining - 32 > 0) begin
        getWord(2'b0);
      end

      // only executes once after final word is fetched from message
      else if(!delimited) begin
        delimited <= 1;
        getWord(2'b1);
      end        

      // if no more values, but new block needs to be padded
      else if(padding > 0) begin
        getWord(2'b2);       
      end
     
      // if block is padded, and now needs size
      else begin
        getWord(2'b3);
      end

      gettingBlock <= 0;
      processingBlock <= 1;

    end    


    //** PROCESSING BLOCK
    else if(!waiting && !gettingBlock && processingBlock && !writing) begin
      
      // If still processing a 512 bit block
      if(t < 64) begin

        {A,B,C,D,E,F,G,H} <= processBlock();
        t <= t + 1;
        mem_addr <= message_addr + (16)*blockNumber + t;
        processingBlock <= 0;
        gettingBlock <= 1;
      end

      // If completed processing 512 bit block
      else begin

        h[0] <= h[0] + A;
        h[1] <= h[1] + B;
        h[2] <= h[2] + C;
        h[3] <= h[3] + D;
        h[4] <= h[4] + E;
        h[5] <= h[5] + F;
        h[6] <= h[6] + G;
        h[7] <= h[7] + H;

        if(allProcessed) begin
          writing <= 1;
          processingBlock <= 0;
        end

        else begin
          gettingBlock <= 1;
          processingBlock <= 0;
          t <= 0;
          blockNumber <= blockNumber + 1;
        end
      end
    end     




    //** WRITING STATE
    // Only runs after all blocks are processed
    else if(!waiting && !gettingBlock && !processingBlock && writing) begin

      mem_addr <= output_addr + t;
      mem_we <= 1;
      mem_write_data <= h[t];
      t <= t + 1;

      // If all 32-bit words written, throw done flag
      if(t == 8) begin
        done <= 1;      
      end
    end



    //** WAITING STATE
    else begin
      // Do nothing
    end


  end

  /** FUNCTIONS **/

  
  // function to return word from message block
  function bit [31:0] getWord(bit [1:0] pad);
  
    bit [5:0] index;
    bit [31:0] temp;

    // Return t'th 32-bit word if t < 16
    if(t < 16) begin

      // Populate W with words from message
      if(pad == 0) begin
        W[15] <= mem_read_data;
        remaining <= remaining - 32;
      end
 
      // Populate W with a delimiter (hex 80)
      else if(pad == 1) begin
        if(remaining > 0) begin
          temp = mem_read_data;
          temp = temp >> (32 - remaining);
          temp = temp << 1;
          temp[0] = 1;
          temp = temp << (31 - remaining);
          W[15] <= temp;
          padding <= padding - ((32-remaining)-1);
        end
        else begin
        W[15] <= 32'h80;
        end
      end

      // Populate W with zeros
      else if(pad == 2) begin
        W[15] <= 32'h0;
        padding <= padding - 32;
      end

      else begin
        if(!firstSize) begin
          W[15] <= 32'b0;
          firstSize <= 1;
        end
        else begin
          W[15] <= size;
          allProcessed <= 1;
        end
      end
    end

    else begin

      s0 = ({W[1][6:0],W[1][31:7]}) ^ ({W[1][17:0],W[1][31:18]}) ^ (W[1]>>3);
      s1 = ({W[14][16:0],W[14][31:17]}) ^ ({W[14][18:0],W[14][31:19]}) ^ (W[14]>>10);
      W[15] = W[0] + s0 + W[9] + s1;
    end

    for(index = 0; index < 15; index = index + 1) begin
        W[index] <= W[index+1];
    end
  endfunction


  // function for a round of processing
  function bit [255:0] processBlock();
    
    S0 = ({A[1:0],A[31:2]}) ^ ({A[12:0],A[31:13]}) ^ ({A[21:0],A[31:22]});
    maj = (A & B) ^ (A & C) ^ (B & C);
    t2 = S0 + maj;
    S1 = ({E[5:0],E[31:6]}) ^ ({E[10:0],E[31:11]}) ^ ({E[24:0],E[31:25]});
    ch = (E & F) ^ ((~E) & G);
    t1 = H + S1 + ch + K[t] + W[15];
    return {t1+t2,A,B,C,D+t1,E,F,G};
  endfunction
    

endmodule
