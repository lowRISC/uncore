// Unitest test bench for tagcache

`include "tagcache.consts.vh"

module tb;

`ifdef N_L2_TRACKERS
   localparam L2Xacts = `N_L2_TRACKERS;
   localparam TLCIS = 7;
   localparam TLMIS = 3;
`else
   localparam L2Xacts = 1;
   localparam TLCIS = 5;
   localparam TLMIS = 1;
`endif
   localparam [63:0] MemSize = `ROCKET_MEM_SIZE;
   localparam [63:0] MemBase = `ROCKET_MEM_BASE;
   localparam MemAW   = 32;
   localparam MemDW   = 64;
   localparam MemTW   = 4;
   localparam MemBS   = 8;      // burst size
   localparam TLAW    = 32;
   localparam TLDW    = 64;
   localparam TLTW    = 4;
   localparam TLBS    = 8;      // burst size

   //global records of transactions
   bit [TLAW-1:0] addr_queue[$] = {};

class TCXact;
   rand int unsigned             id;
   rand bit [1:0]                addr_type;
   rand int                      addr_offset;
   bit [TLAW-1:0]                addr;
   rand bit [TLBS-1:0][TLDW-1:0] data;
   rand bit [TLBS-1:0][TLTW-1:0] tag;
   rand bit                      rw;
   rand bit                      burst;
   rand bit                      zero_tag;

   // address types:
   //   0: just the last one
   //   1: within neighbour block
   //   2: within the same page
   //   3: anywhere
   constraint type_constraint { addr_type dist {0 := 3, 1 := 6, 2 := 3, 3 := 2}; }
   constraint offset_constraint {
     addr_type == 0 -> (addr_offset == 0);
     addr_type == 1 -> (addr_offset < 64 && addr_offset > -64);
     addr_type == 2 -> (addr_offset < 64*1024 && addr_offset > 64*1024);
   }
   constraint size_constraint { burst dist {0 := 2, 1 := 6}; }
   constraint id_constraint { id < L2Xacts; }
   constraint tag_constraint { zero_tag dist {0 := 1, 1 := 3}; }

   function string toString(int resp = 0);
      string operation_str = rw ? "write" : "read";
      string size_str = burst ? "a block" : "a beat";
      int index = addr[5:0]/(TLDW/8);
      string beat_data = $sformatf("%8h,%1h", data[index], tag[index]);
      string data_str = burst ? block_data : beat_data;
      string block_data;
      foreach(data[i]) begin
         if(i==0) block_data = $sformatf("%8h,%1h", data[0], tag[0]);
         else     block_data = {$sformatf("%8h,%1h ", data[i], tag[i]), block_data};
      end
      if(resp)
        return {$sformatf("L2 tracker %d response ",id), operation_str, " ", size_str, " @ 0x", $sformatf("%8h",addr), !rw ? {" with data: ", data_str} : "" };
      else
        return {$sformatf("L2 tracker %d request ",id), operation_str, " ", size_str, " @ 0x", $sformatf("%8h",addr), rw ? {" with data: ", data_str} : "" };
   endfunction // toString

   function void post_randomize();
      if(addr_queue.size == 0)
        rw = 1;

      if(rw == 1) begin         // write
         addr = addr_queue[0] - MemBase + addr_offset;
         addr = addr % MemSize;
         addr = addr < 0 ? addr + MemSize : addr;
         addr = burst ? addr / 64 * 64 : addr / (TLDW/8) * (TLDW/8);
         addr = addr + MemBase;
         if(zero_tag) tag = 0;
      end else begin            // read
         int index = addr_offset < 0 ? -addr_offset/64 : addr_offset/64;
         index = index % addr_queue.size;
         addr = burst ? addr_queue[index] : addr_queue[index] + (addr_offset % 64) / (TLDW/8) * (TLDW/8);
      end
   endfunction // post_randomize

endclass

   typedef bit [TLBS-1:0][TLDW-1:0] cache_data_t;
   typedef bit [TLBS-1:0][TLTW-1:0] cache_tag_t;
   typedef bit [TLAW-1:0] addr_t;
   cache_data_t memory_data_map [addr_t];
   cache_tag_t  memory_tag_map  [addr_t];

   TCXact xact_queue[$] = {};
   mailbox   send_queue = new(L2Xacts*2);
   mailbox   recv_queue = new;

   function record_xact(input TCXact xact);
      bit [TLAW-1:0] addr = xact.addr /  64 * 64;
      int unsigned   index = xact.addr[5:0]/(TLDW/8);

      addr_queue.push_front(addr);
      if(addr_queue.size > 1024) addr_queue.pop_back();

      xact_queue.push_front(xact);
   endfunction // record_xact

   task check_xact(input TCXact xact);
      TCXact orig_xact;
      bit [TLAW-1:0] addr;
      int unsigned   index;
      int qi[$] = xact_queue.find_last_index(x) with (x.id == xact.id);
      if(qi.size == 0)
           $fatal(1, "Get a response to an unknown transaction!n");
      orig_xact = xact_queue[qi[0]];
      addr = orig_xact.addr /  64 * 64;
      index = xact.addr[5:0]/(TLDW/8);
      if(xact.rw) begin         // write
         // update the data
         if(!memory_data_map.exists(addr)) begin
            memory_data_map[addr] = 0;
            memory_tag_map[addr] = 0;
         end
         if(orig_xact.burst) begin
            memory_data_map[addr] = orig_xact.data;
            memory_tag_map[addr]  = orig_xact.tag;
         end else begin
            memory_data_map[addr][index] = orig_xact.data[index];
            memory_tag_map[addr][index]  = orig_xact.tag[index];
         end
      end else begin            // read
         if(!memory_data_map.exists(addr))
           $fatal(1, "Read response miss in memory map!\n");
         if(xact.burst && (memory_data_map[addr] != xact.data || memory_tag_map[addr] != xact.tag))
           $fatal(1, "Read response mismatch with memory map!\n");
         if(!xact.burst && (memory_data_map[addr][index] != xact.data[index] || memory_tag_map[addr][index] != xact.tag[index]))
           $fatal(1, "Read response mismatch with memory map!\n");
      end // else: !if(xact.rw)
      xact_queue.delete(qi[0]);
   endtask // check_xact

   task xact_gen();
      while(1) begin
         TCXact xact = new;
         xact.randomize();
         record_xact(xact);
         send_queue.put(xact);
      end
   endtask // xact_gen

   task xact_check();
      while(1) begin
         TCXact xact;
         recv_queue.get(xact);
         check_xact(xact);
      end
   endtask // xact_check


   reg              clk, reset, init;
   logic            io_in_acquire_ready;
   reg              io_in_acquire_valid = 'b0;
   reg [TLAW-7:0]   io_in_acquire_bits_addr_block;
   reg [TLCIS-1:0]  io_in_acquire_bits_client_xact_id;
   reg [2:0]        io_in_acquire_bits_addr_beat;
   reg              io_in_acquire_bits_is_builtin_type;
   reg [2:0]        io_in_acquire_bits_a_type;
   reg [12:0]       io_in_acquire_bits_union;
   reg [TLDW-1:0]   io_in_acquire_bits_data;
   reg [TLTW-1:0]   io_in_acquire_bits_tag;
   reg              io_in_acquire_bits_client_id;
   reg              io_in_grant_ready;
   logic            io_in_grant_valid;
   logic [2:0]      io_in_grant_bits_addr_beat;
   logic [TLCIS-1:0]io_in_grant_bits_client_xact_id;
   logic [TLMIS-1:0]io_in_grant_bits_manager_xact_id;
   logic            io_in_grant_bits_is_builtin_type;
   logic [3:0]      io_in_grant_bits_g_type;
   logic [TLDW-1:0] io_in_grant_bits_data;
   logic [TLTW-1:0] io_in_grant_bits_tag;
   logic            io_in_grant_bits_client_id;
   logic            io_in_finish_ready;
   reg              io_in_finish_valid = 'b0;
   reg [TLMIS-1:0]  io_in_finish_bits_manager_xact_id;
   reg              io_in_probe_ready;
   logic            io_in_probe_valid;
   logic [TLAW-7:0] io_in_probe_bits_addr_block;
   logic            io_in_probe_bits_p_type;
   logic            io_in_probe_bits_client_id;
   logic            io_in_release_ready;
   reg              io_in_release_valid = 'b0;
   reg [2:0]        io_in_release_bits_addr_beat;
   reg [TLAW-7:0]   io_in_release_bits_addr_block;
   reg [TLCIS-1:0]  io_in_release_bits_client_xact_id;
   reg              io_in_release_bits_voluntary;
   reg [1:0]        io_in_release_bits_r_type;
   reg [TLDW-1:0]   io_in_release_bits_data;
   reg [TLTW-1:0]   io_in_release_bits_tag;
   reg              io_in_release_bits_client_id;

   initial begin
      reset = 'b0;
      init = 'b0;
      #33;
      reset = 'b1;
      init = 'b1;
      #79;
      reset = 0;
   end

   initial begin
      clk = 0;
      forever #5 clk = !clk;
   end

   nasti_channel
     #(
       .ID_WIDTH    ( 8       ),
       .ADDR_WIDTH  ( MemAW   ),
       .DATA_WIDTH  ( MemDW   ))
   mem_nasti();

   nasti_ram_behav
     #(
       .ID_WIDTH     ( 8       ),
       .ADDR_WIDTH   ( MemAW   ),
       .DATA_WIDTH   ( MemDW   ),
       .USER_WIDTH   ( 1       )
       )
   ram_behav
     (
      .clk           ( clk         ),
      .rstn          ( !reset      ),
      .nasti         ( mem_nasti   )
      );

   task xact_send();
      while(1) begin
         TCXact xact;
         @(posedge clk); #1;
         io_in_acquire_valid = 'b0;
         send_queue.get(xact);

         if(xact.rw && xact.burst) begin      // write a burst
            foreach(xact.data[i]) begin
               @(posedge clk); #1;
               io_in_acquire_valid = 'b1;
               io_in_acquire_bits_addr_block = xact.addr >> 6;
               io_in_acquire_bits_client_xact_id = xact.id;
               io_in_acquire_bits_addr_beat = i;
               io_in_acquire_bits_is_builtin_type = 'b1;
               io_in_acquire_bits_a_type = 'b011;
               io_in_acquire_bits_union = {{TLDW/64*4{1'b1}}, {TLDW/8{1'b1}}, 1'b1};
               io_in_acquire_bits_data = xact.data[i];
               io_in_acquire_bits_tag = xact.tag[i];
               #5; if(!io_in_acquire_ready) @(posedge io_in_acquire_ready);
            end // foreach (xact.data[i])
         end // if (xact.rw && xact.burst)

         if(xact.rw && !xact.burst) begin      // write a beat
            int beat = xact.addr[5:0] / (TLDW/8);
            @(posedge clk); #1;
            io_in_acquire_valid = 'b1;
            io_in_acquire_bits_addr_block = xact.addr >> 6;
            io_in_acquire_bits_client_xact_id = xact.id;
            io_in_acquire_bits_addr_beat = beat;
            io_in_acquire_bits_is_builtin_type = 'b1;
            io_in_acquire_bits_a_type = 'b010;
            io_in_acquire_bits_union = {{TLDW/64*4{1'b1}}, {TLDW/8{1'b1}}, 1'b1};
            io_in_acquire_bits_data = xact.data[beat];
            io_in_acquire_bits_tag = xact.tag[beat];
            #5; if(!io_in_acquire_ready) @(posedge io_in_acquire_ready);
         end // if (xact.rw && !xact.burst)

         if(!xact.rw && xact.burst) begin      // read a block
            @(posedge clk); #1;
            io_in_acquire_valid = 'b1;
            io_in_acquire_bits_addr_block = xact.addr >> 6;
            io_in_acquire_bits_client_xact_id = xact.id;
            io_in_acquire_bits_is_builtin_type = 'b1;
            io_in_acquire_bits_a_type = 'b001;
            io_in_acquire_bits_union = {4'b0111, 5'b00000, 1'b1};
            #5; if(!io_in_acquire_ready) @(posedge io_in_acquire_ready);
         end // if (xact.rw && !xact.burst)

         if(!xact.rw && !xact.burst) begin      // read a beat
            int beat = xact.addr[5:0] / (TLDW/8);
            @(posedge clk); #1;
            io_in_acquire_valid = 'b1;
            io_in_acquire_bits_addr_block = xact.addr >> 6;
            io_in_acquire_bits_addr_beat = beat;
            io_in_acquire_bits_client_xact_id = xact.id;
            io_in_acquire_bits_is_builtin_type = 'b1;
            io_in_acquire_bits_a_type = 'b000;
            io_in_acquire_bits_union = {4'b0111, 5'b00000, 1'b1};
            #5; if(!io_in_acquire_ready) @(posedge io_in_acquire_ready);
         end // if (xact.rw && !xact.burst)

      end // while (1)
   endtask // xact_send

   task xact_recv();
      while(1'b1) begin
         TCXact xact = new;
         @(posedge clk); #1;
         io_in_grant_ready = 'b1;
         #5; if(!io_in_grant_valid) @(posedge io_in_grant_valid);

         if(io_in_grant_bits_g_type == 4'b011) begin
            xact.rw = 'b1;
            xact.burst = 'b0;
            xact.id = io_in_grant_bits_client_xact_id;
            xact.addr = io_in_grant_bits_addr_beat * (TLDW/8);
         end

         if(io_in_grant_bits_g_type == 4'b100) begin
            xact.rw = 'b0;
            xact.burst = 'b0;
            xact.id = io_in_grant_bits_client_xact_id;
            xact.addr = io_in_grant_bits_addr_beat * (TLDW/8);
            xact.data[io_in_grant_bits_addr_beat] = io_in_grant_bits_data;
            xact.tag[io_in_grant_bits_addr_beat] = io_in_grant_bits_tag;
         end
            
         if(io_in_grant_bits_g_type == 4'b101) begin
            int i;
            xact.rw = 'b0;
            xact.burst = 'b1;
            xact.id = io_in_grant_bits_client_xact_id;
            xact.addr = 0;
            xact.data[io_in_grant_bits_addr_beat] = io_in_grant_bits_data;
            xact.tag[io_in_grant_bits_addr_beat] = io_in_grant_bits_tag;
            for (i=1; i<TLBS; i=i+1) begin
               @(posedge clk); #1;
               #5; if(!io_in_grant_valid) @(posedge io_in_grant_valid);
               xact.data[io_in_grant_bits_addr_beat] = io_in_grant_bits_data;
               xact.tag[io_in_grant_bits_addr_beat] = io_in_grant_bits_tag;
            end
         end // if (io_in_grant_bits_g_type == 4'b101)

         @(posedge clk); #1;
         io_in_grant_ready = 'b0;
         recv_queue.put(xact);
      end
   endtask // xact_recv

   TagCacheTop DUT
     (
      .*,
      .io_out_aw_valid         ( mem_nasti.aw_valid                     ),
      .io_out_aw_ready         ( mem_nasti.aw_ready                     ),
      .io_out_aw_bits_id       ( mem_nasti.aw_id                        ),
      .io_out_aw_bits_addr     ( mem_nasti.aw_addr                      ),
      .io_out_aw_bits_len      ( mem_nasti.aw_len                       ),
      .io_out_aw_bits_size     ( mem_nasti.aw_size                      ),
      .io_out_aw_bits_burst    ( mem_nasti.aw_burst                     ),
      .io_out_aw_bits_lock     ( mem_nasti.aw_lock                      ),
      .io_out_aw_bits_cache    ( mem_nasti.aw_cache                     ),
      .io_out_aw_bits_prot     ( mem_nasti.aw_prot                      ),
      .io_out_aw_bits_qos      ( mem_nasti.aw_qos                       ),
      .io_out_aw_bits_region   ( mem_nasti.aw_region                    ),
      .io_out_aw_bits_user     ( mem_nasti.aw_user                      ),
      .io_out_w_valid          ( mem_nasti.w_valid                      ),
      .io_out_w_ready          ( mem_nasti.w_ready                      ),
      .io_out_w_bits_data      ( mem_nasti.w_data                       ),
      .io_out_w_bits_id        (                                        ),
      .io_out_w_bits_strb      ( mem_nasti.w_strb                       ),
      .io_out_w_bits_last      ( mem_nasti.w_last                       ),
      .io_out_w_bits_user      ( mem_nasti.w_user                       ),
      .io_out_b_valid          ( mem_nasti.b_valid                      ),
      .io_out_b_ready          ( mem_nasti.b_ready                      ),
      .io_out_b_bits_id        ( mem_nasti.b_id                         ),
      .io_out_b_bits_resp      ( mem_nasti.b_resp                       ),
      .io_out_b_bits_user      ( mem_nasti.b_user                       ),
      .io_out_ar_valid         ( mem_nasti.ar_valid                     ),
      .io_out_ar_ready         ( mem_nasti.ar_ready                     ),
      .io_out_ar_bits_id       ( mem_nasti.ar_id                        ),
      .io_out_ar_bits_addr     ( mem_nasti.ar_addr                      ),
      .io_out_ar_bits_len      ( mem_nasti.ar_len                       ),
      .io_out_ar_bits_size     ( mem_nasti.ar_size                      ),
      .io_out_ar_bits_burst    ( mem_nasti.ar_burst                     ),
      .io_out_ar_bits_lock     ( mem_nasti.ar_lock                      ),
      .io_out_ar_bits_cache    ( mem_nasti.ar_cache                     ),
      .io_out_ar_bits_prot     ( mem_nasti.ar_prot                      ),
      .io_out_ar_bits_qos      ( mem_nasti.ar_qos                       ),
      .io_out_ar_bits_region   ( mem_nasti.ar_region                    ),
      .io_out_ar_bits_user     ( mem_nasti.ar_user                      ),
      .io_out_r_valid          ( mem_nasti.r_valid                      ),
      .io_out_r_ready          ( mem_nasti.r_ready                      ),
      .io_out_r_bits_id        ( mem_nasti.r_id                         ),
      .io_out_r_bits_data      ( mem_nasti.r_data                       ),
      .io_out_r_bits_resp      ( mem_nasti.r_resp                       ),
      .io_out_r_bits_last      ( mem_nasti.r_last                       ),
      .io_out_r_bits_user      ( mem_nasti.r_user                       )
      );
   
endmodule // tagcache_tab

