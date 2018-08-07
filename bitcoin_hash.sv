

module bitcoin_hash(
  input logic clk, reset_n, start,
  input logic [31:0] message_addr, output_addr,
  output logic done, mem_clk, mem_we,
  output logic [15:0] mem_addr,
  output logic [31:0] mem_write_data,
  input logic [31:0] mem_read_data);
  
  parameter NUM_NONCES = 16;

  int i;
  int n;

  logic [31:0] nonce;

  logic [31:0] header_hash[8];

  logic [31:0] firstBlockHash [8];

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
  bit halt;// a state to delay clock cycle in order to wait on memory clock
  bit allProcessed;
  bit prepRoundTwo;
  bit roundTwo;
  bit startRoundTwo;
  logic start_state;
  bit startNextNonce;
  bit prepGetNewNonce;

  bit [7:0] t;// index variable  
  bit [63:0] blockNumber;// indicates number of 512 bit block that is being processed
  logic [31:0] W[16];// storage for each w_t so they don't need to be calculated multiple times.
  logic [31:0] h[8];

  logic [31:0] size;

  logic [7:0] nonces_processed;


  bit [31:0] remaining;// # of bits left to be fetched
  bit delimited = 0;// indicates whether or not a delimiter has been implanted in the message
  bit [31:0] padding;// indicates the number of 0s needed to pad the message

  // Temporary variables
  logic [31:0] A,B,C,D,E,F,G,H,
               s0,s1,
               maj,ch,
               t1,t2,
               S0,S1;

  // set memory clock
  assign mem_clk = clk;
  
  always_comb begin
    start_state <= start | startRoundTwo;
  end

  // sequential logic to be performed with the clock
  always_ff @(posedge clk) begin
    mem_we <= 0;

    //** START
    // Turn off waiting upon start flag
    if(start_state) begin

      waiting <= 0;
      h[0] <= 'h6a09e667;
      h[1] <= 'hbb67ae85;
      h[2] <= 'h3c6ef372;
      h[3] <= 'ha54ff53a;
      h[4] <= 'h510e527f;
      h[5] <= 'h9b05688c;
      h[6] <= 'h1f83d9ab;
      h[7] <= 'h5be0cd19;

      A <= 'h6a09e667;
      B <= 'hbb67ae85;
      C <= 'h3c6ef372;
      D <= 'ha54ff53a;
      E <= 'h510e527f;
      F <= 'h9b05688c;
      G <= 'h1f83d9ab;
      H <= 'h5be0cd19;


      if(roundTwo) begin
        remaining <= 256;
        size <= 32;
        startRoundTwo <= 0;
        padding <= 512 - (((32*8)%512)+65);
      end
      else begin
        remaining <= 640;
        size <= 80;
        // Determine size of padding
        padding <= 512 - (((80*8)%512)+65);
      end

      done <= 0;
      halt <= 1;
      mem_addr <= message_addr;// get first mem address
      allProcessed <= 0;
      delimited <= 0;
      nonces_processed <= 0;

      if(start) nonce <= 0;
     
    end
    

    //** Halt a cycle, used for waiting on memory clock
    else if(halt) begin
      halt <= 0;
      gettingBlock <= 1;
    end



    //** GETTINGBLOCK STATE
    else if(gettingBlock) begin

      // if values remain to be fetched
      if(!(((remaining - 32)>>31)&1)) begin

        if(t < 16) begin
          W[15] <= getWord(0);
          remaining <= remaining - 32;

          if(blockNumber == 1 && t == 3) begin
            W[15] <= nonce;
          end

        end
        else if(t < 64) begin
          W[15] <= getWord(0);
        end

      end

      // only executes once after final word is fetched from message
      else if(!delimited) begin

        delimited <= 1;
        if(remaining > 0) begin
          W[15] <= getWord(1);
          padding <= padding - ((32-remaining)-1);
        end
        else begin

          W[15] <= 32'h80000000;
        end
      end        

      // if no more values, but new block needs to be padded
      else if(!padding[31]) begin

        if(t < 16) padding <= padding - 32;
        W[15] <= getWord(2);       
      end
     
      // if block is padded, and now needs size
      else begin

       if(t == 15) begin

          W[15] <= size*8;
          allProcessed <= 1;
        end
        else begin
          W[15] <= getWord(2);
        end
      end
 
     for(int index = 0; index < 15; index = index + 1) begin
       W[index] <= W[index+1];
     end
      gettingBlock <= 0;
      processingBlock <= 1;

    end    


    //** PROCESSING BLOCK
    else if(processingBlock) begin

      // If still processing a 512 bit block
      if(t < 64) begin
        {A,B,C,D,E,F,G,H} <= processBlock();
        t <= t + 1;
        mem_addr <= message_addr + (16)*blockNumber + t + 1;
        processingBlock <= 0;
        halt <= 1;
      end

      // If completed processing 512 bit block
      else begin 
//$stop;
        h[0] <= h[0] + A;
        h[1] <= h[1] + B;
        h[2] <= h[2] + C;
        h[3] <= h[3] + D;
        h[4] <= h[4] + E;
        h[5] <= h[5] + F;
        h[6] <= h[6] + G;
        h[7] <= h[7] + H;

        if(blockNumber == 0 && !nonce && !allProcessed) begin

        // store this for later
          header_hash[0] <= h[0] + A;
          header_hash[1] <= h[1] + B;
          header_hash[2] <= h[2] + C;
          header_hash[3] <= h[3] + D;
          header_hash[4] <= h[4] + E;
          header_hash[5] <= h[5] + F;
          header_hash[6] <= h[6] + G;
          header_hash[7] <= h[7] + H;
        end


        if(allProcessed) begin
          writing <= 1;
          t <= 0;
          processingBlock <= 0;
        end

        // get new Mj
        else begin

          halt <= 1;
          processingBlock <= 0;
          t <= 0;
          blockNumber <= blockNumber + 1;
          mem_addr <= message_addr + 16*(blockNumber+1);

          A <= h[0] + A;
          B <= h[1] + B;
          C <= h[2] + C;
          D <= h[3] + D;
          E <= h[4] + E;
          F <= h[5] + F;
          G <= h[6] + G;
          H <= h[7] + H;
        end
      end
    end     




    //** WRITING STATE
    // Only runs after all blocks are processed
    else if(writing) begin

      if(!roundTwo) begin
        // Save message digest to original memory location
        mem_addr <= message_addr + t;
        mem_we <= 1;
        mem_write_data <= h[t];
        t <= t + 1;

        // If all 32-bit words written, run SHA again
        if(t == 8) begin
          prepRoundTwo <= 1;      
          writing <= 0;
        end
      end
      else begin

        // write message digest for nonce to memory, start next nonce processing
        if(nonce < 16) begin
//$stop;
          mem_addr <= output_addr + nonce;
          mem_we <= 1;
          mem_write_data <= h[0];
          t <= 0;
          nonce <= nonce + 1;
          blockNumber <= 1;

          h[0] <= header_hash[0];
          h[1] <= header_hash[1];
          h[2] <= header_hash[2];
          h[3] <= header_hash[3];
          h[4] <= header_hash[4];
          h[5] <= header_hash[5];
          h[6] <= header_hash[6];
          h[7] <= header_hash[7];
        
          A <= header_hash[0];
          B <= header_hash[1];
          C <= header_hash[2];
          D <= header_hash[3];
          E <= header_hash[4];
          F <= header_hash[5];
          G <= header_hash[6];
          H <= header_hash[7];

         

          writing <= 0;
          prepGetNewNonce <= 1;

        end
        else begin
          done <= 1;
        end
      end
    end

    // this is sha(sha(message)), reset all to same as starting state
    else if(prepRoundTwo) begin


      roundTwo <= 1;
      t <= 0;
      prepRoundTwo <= 0;
      startRoundTwo <= 1;
      blockNumber <= 0;
   
    end

    else if(prepGetNewNonce) begin
//$stop;
      mem_addr <= message_addr + 16;
      prepGetNewNonce <= 0;
      halt <= 1;
      roundTwo <= 0;
      size <= 80;
      remaining <= 128;
      padding <= 512 - (((80*8)%512)+65);
      delimited <= 0;
    end

    //** WAITING STATE
    else begin
      // Do nothing
    end


  end

  /** FUNCTIONS **/

  
  // function to return word from message block
  function bit [31:0] getWord(bit [1:0] pad);
  
    bit [31:0] temp;

    // Return t'th 32-bit word if t < 16
    if(t < 16) begin

      // Populate W with words from message
      if(pad == 0) begin
        return mem_read_data;
      end
 
      // Populate W with a delimiter (hex 80)
      else if(pad == 1) begin
        temp = mem_read_data;
        temp = temp >> (32 - remaining);
        temp = temp << 1;
        temp[0] = 1;
        temp = temp << (31 - remaining);
        return temp;       
      end

      // Populate W with zeros
      else if(pad == 2) begin
        return 32'h0;
      end
    end

    else begin
      s0 = ({W[1][6:0],W[1][31:7]}) ^ ({W[1][17:0],W[1][31:18]}) ^ (W[1]>>3);
      s1 = ({W[14][16:0],W[14][31:17]}) ^ ({W[14][18:0],W[14][31:19]}) ^ (W[14]>>10);
      return W[0] + s0 + W[9] + s1;
    end

  endfunction


  // function for a round of processing
  function bit [255:0] processBlock();
   // if(t > 62) $stop;
    S0 = ({A[1:0],A[31:2]}) ^ ({A[12:0],A[31:13]}) ^ ({A[21:0],A[31:22]});
    maj = (A & B) ^ (A & C) ^ (B & C);
    t2 = S0 + maj;
    S1 = ({E[5:0],E[31:6]}) ^ ({E[10:0],E[31:11]}) ^ ({E[24:0],E[31:25]});
    ch = (E & F) ^ ((~E) & G);
    t1 = H + S1 + ch + K[t] + W[15];
    return {t1+t2,A,B,C,D+t1,E,F,G};
  endfunction
    
endmodule
