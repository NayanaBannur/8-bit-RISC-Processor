module testbench;
reg clk, reset;

initial
	begin
	clk = 1'b0;
	reset = 1'b1;
	reset = #161 1'b0;
	end

always clk = #5 ~clk;

processor_top proc_top1(clk, reset);

endmodule


module processor_top (clk, reset);

	input clk, reset;

	wire 	 clk, store_to_mem,reg_wr_en;
	wire [9:0]  prog_ctr;
	wire	[15:0] instr_mem_out;
	wire	[2:0]  op1_addr, op2_addr,destination_reg_addr;
	wire [7:0]  op1_rd_data, op2_rd_data, mem_data;
	wire [7:0]  data_rd_addr, data_wr_addr;
 	wire [7:0]  datamem_rd_data, datamem_wr_data;
	wire [7:0]  operation_result; 

 
	instr_and_data_mem  mem1(clk, prog_ctr, instr_mem_out, data_rd_addr, data_wr_addr, datamem_rd_data, datamem_wr_data, store_to_mem);

	processor_core proc1(clk,reset,op1_rd_data,op2_rd_data, instr_mem_out,op1_addr, op2_addr,prog_ctr, store_to_mem, reg_wr_en, data_rd_addr, data_wr_addr, datamem_rd_data, datamem_wr_data, operation_result, destination_reg_addr );

	register_file regfile1(clk, reset, datamem_wr_data, op1_rd_data, op2_rd_data, op1_addr, op2_addr, destination_reg_addr, reg_wr_en);

endmodule
	

module instr_and_data_mem (clk, prog_ctr, instr_mem_out, data_rd_addr, data_wr_addr, datamem_rd_data, datamem_wr_data, store_to_mem);

	input clk;
	input [9:0] prog_ctr;
	input [7:0] data_rd_addr, datamem_wr_data;
	input [7:0] data_wr_addr;
	input store_to_mem;  
                                     
	output [15:0] instr_mem_out;
	output [7:0] datamem_rd_data;

	reg [15:0]	instr_mem_out;              
	reg [7:0]	datamem_rd_data;



// instruction memory operations
	reg [15:0] instr_mem[0:1023];

	
// load program into memory 
	initial
	begin
	$readmemh("C:/Users/ekstep1/Desktop/verlog/run/program2.txt",instr_mem);
	end

// read instructions from memory
	always @(posedge clk)
		instr_mem_out <=  #1 instr_mem[prog_ctr];


// data memory operations
	reg [7:0] data_mem[255:0];

// initialize data memory from file
	initial
	begin
	$readmemh("C:/Users/ekstep1/Desktop/verlog/run/data2.txt",data_mem);
	end
                                                     
// get data during LOAD instruction                 
	always @(data_rd_addr)
		datamem_rd_data <= data_mem[data_rd_addr]; 
                                                      
// write to data memory in STORE instruction
	always @(posedge clk)
		if (store_to_mem == 1'b1)
			data_mem[data_wr_addr] <= datamem_wr_data;

endmodule


module processor_core (clk,reset,op1_rd_data,op2_rd_data, instr_mem_out,op1_addr,op2_addr,prog_ctr,store_to_mem, reg_wr_en, data_rd_addr, data_wr_addr, datamem_rd_data, datamem_wr_data, operation_result,destination_reg_addr);               

	input	clk, reset;
	input [7:0]  op1_rd_data, op2_rd_data;
	input [7:0]  datamem_rd_data;
	input [15:0] instr_mem_out;

	output [2:0] op1_addr, op2_addr;
	output [9:0] prog_ctr;
	output [7:0] datamem_wr_data;
	output [7:0] data_rd_addr, data_wr_addr;
	output [7:0] operation_result;
	output [2:0] destination_reg_addr;
	output 	     store_to_mem, reg_wr_en;

	reg [2:0] op1_addr, op2_addr, op1_addr_reg , op2_addr_reg;
	reg [9:0] branch_addr,prog_ctr,nxt_prog_ctr;
	reg [9:0] nxt_prog_ctr_reg, nxt_prog_ctr_r2;
	reg [7:0] op1_data_reg, op2_data_reg; 
	reg [7:0] operation_result,data_rd_addr;
	reg [15:0] instruction;
	reg [4:0] opcode;
	reg [7:0] ld_mem_addr;
	reg [7:0] data_wr_addr;
	reg [2:0] destination_reg_addr ;
	reg [7:0] logical_data_out;
	reg [7:0] data_in1,data_in2;
	reg [7:0] ld_mem_addr_reg;
	reg [7:0] alu_data_out,shift_data_out;
	reg [7:0] data_out_reg, res_addr, res_addr_reg;
	reg [7:0] st_mem_addr, st_mem_addr_reg;
	reg		add_op_true, carry_in, en_op2_complement;
	reg		and_op_true, or_op_true, not_op_true;
	reg	     	jump_true, shift_left_true,store_to_mem_ex ;
	reg		add_op_true_reg, carry_in_reg;
	reg		en_op2_complement_reg,jump_true_reg;       
	reg		and_op_true_reg,or_op_true_reg,not_op_true_reg;
	reg		shift_left_true_reg;
	reg		set_invalidate_instruction;
	reg		carry_flag, save_carry_flag_reg;
	reg		lgcl_or_bitwse_T;
	reg		lgcl_or_bitwse_T_reg;
	reg		load_true, store_true;
	reg		write_to_regfile, write_to_regfile_reg;
	reg		store_true_reg, load_true_reg;
	reg		alu_carry_out, shift_carry_out;
	reg		load_mem_data;
	reg		bypass_op1_ex_stage, bypass_op2_ex_stage;
	reg		bypass_op1_dcd_stage, bypass_op2_dcd_stage; 
	reg		and_bitwise_true_reg, branch_taken_reg;
	reg		and_bitwise_true, or_bitwise_true_reg;
	reg		or_bitwise_true, not_bitwsie_true_reg;
	reg		not_bitwise_true, not_bitwise_true_reg;
	reg		invalidate_fetch_instr;
	reg		invalidate_decode_instr_r1;
	reg		invalidate_fetch_instr_r2;
	reg		invalidate_decode_instr,reg_wr_en_ex;
	reg		invalidate_fetch_instr_r1;
	reg		invalidate_execute_instr,carry_flag_ex;
	reg		gt_flag_ex_reg, gt_flag;
	reg		lt_flag_ex_reg, lt_flag, unconditional_jump;
	reg		eq_flag_ex_reg, eq_flag, unconditional_jump_reg;
	reg		compare_true, compare_true_reg, compare_true_r2;
	reg		jump_gt, jump_lt, jump_eq, jump_carry;
	reg		jump_gt_reg, jump_lt_reg, jump_eq_reg;
	reg		jump_carry_reg;

	wire [7:0] datamem_wr_data, data_out;
	wire [7:0] operand1, operand2;
	wire [7:0] op1_data, op2_data, branch_taken;
	wire		carry_out,save_carry_flag;
	wire		invalidate_instr, store_to_mem, reg_wr_en;
	wire		gt_flag_ex, lt_flag_ex, eq_flag_ex;
	wire		gt_flag_true, lt_flag_true, eq_flag_true;
	wire		carry_flag_true;


//****************************************
//   RESET INITIALUZATION
//****************************************

//keep instruction and program counter at zero during reset
//Disable register and memory writes during reset
	always @(reset)
	   if (reset == 1'b1)                          
		begin
		prog_ctr <= 10'b0;
		instruction <= 16'b0;
		end

//****************************************
//	INSTRUCTION FETCH PIPELINE REGISTERS
//****************************************
                                                   
	always @(posedge clk)
		if (reset == 1'b1)
		   begin
		   instruction <= 16'b0;
		   invalidate_fetch_instr <= 1'b0;
		   end
		else
		   begin
		   instruction <= #1 instr_mem_out;
		   if (branch_taken_reg == 1'b1)
			invalidate_fetch_instr <= #1 1'b1;
		   else
			invalidate_fetch_instr <= #1 1'b0;

		   end

                                             
//***************************
//	INSTRUCTION DECODE LOGIC
//****************************
	always@(instruction)
		begin
		opcode	<=  instruction[15:11];
		op1_addr	<=  instruction[2:0];
		op2_addr	<= instruction[6:4];
		res_addr	<= instruction[10:8];
		ld_mem_addr <= instruction[7:0];
		st_mem_addr <= instruction [10:3];
		branch_addr <= instruction[9:0];
		end

	always@ (opcode or branch_addr)        
		begin
		add_op_true <= 1'b0;
		and_op_true <= 1'b0;
		or_op_true  <= 1'b0;
		not_op_true <= 1'b0; 
		carry_in	<= 1'b0;
		en_op2_complement  <= 1'b0;
		jump_true	<= 1'b0;
		compare_true <= 1'b0;
		shift_left_true <= 1'b0;
		lgcl_or_bitwse_T <= 1'b0;
		load_true <= 1'b0;
		store_true <= 1'b0;
		write_to_regfile <= 1'b0;
		unconditional_jump <= 1'b0;
		jump_gt <= 1'b0;
		jump_lt <= 1'b0;
		jump_eq <= 1'b0;
		jump_carry <= 1'b0;

		case (opcode)
		//	OP_NOP:  
		//	5'h00:   	;		
		
		//	OP_ADD:	begin
			5'h01:	begin
					write_to_regfile <= 1'b1;
					add_op_true <= 1'b1;
					end
	
		//	OP_SUB:	begin
			5'h02:	begin
					add_op_true <= 1'b1;	
					carry_in	<= 1'b1;
					en_op2_complement <= 1'b1;
					write_to_regfile <= 1'b1;
				   	end

		//	OP_AND:	begin
			5'h03:	begin
					and_op_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1;     
					end

		//	OP_OR:	begin
			5'h04:	begin
					or_op_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1;
					end

		//	OP_NOT:	begin
			5'h05:	begin
					not_op_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1;
					end

		//	OP_SHL	begin                  
			5'h06:	begin
					shift_left_true <= 1'b1;
					write_to_regfile <= 1'b1;
					end

		//	OP_JMP:	begin
			5'h07:	begin
					nxt_prog_ctr <= branch_addr;
					jump_true	<= 1'b1;
					unconditional_jump <= 1'b1;
					end

		//	OP_LOAD:	begin
			5'h08:	begin
					load_true <= 1'b1;
					write_to_regfile <= 1'b1;
					end                      
                                                      
		//	OP_STORE:	store_true <= 1'b1;
			5'h09:	store_true <= 1'b1;

		//	OP_ANDBIT:	begin
			5'h0a:	begin
					and_bitwise_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1; 
			   		end

		//	OP_ORBIT:	begin
			5'h0b:	begin
					or_bitwise_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1;
					end

		//	OP_NOTBIT:	begin
			5'h0c:	begin
					not_bitwise_true <= 1'b1;
					lgcl_or_bitwse_T <= 1'b1;
					write_to_regfile <= 1'b1;
					end
 
		//	OP_COMPARE: begin
			5'h0d:	begin
					add_op_true <= 1'b1;
					compare_true <= 1'b1;	
					carry_in	<= 1'b1;   //subtract
					en_op2_complement <= 1'b1;
				   	end

		//	OP_JMPGT:	begin
			5'h0e:	begin
					nxt_prog_ctr <= branch_addr;
					jump_true	<= 1'b1;
					jump_gt <= 1'b1;
					end

		//	OP_JMPLT:	begin
			5'h0f:	begin
					nxt_prog_ctr <= branch_addr;
					jump_true	<= 1'b1;
					jump_lt <= 1'b1;
					end
		//	OP_JMPEQ:	begin
			5'h10:	begin
					nxt_prog_ctr <= branch_addr;
					jump_true	<= 1'b1;
					jump_eq <= 1'b1;
					end

		//	OP_JMPC:	begin
			5'h11:	begin
					nxt_prog_ctr <= branch_addr;
					jump_true	<= 1'b1;
					jump_carry <= 1'b1;
					end

			default: 	;			//= NOP
			endcase
		end

	
//BYPASS logic
//check store pipeline stage signals and address to determine
// if one of the operands have to be bypassed

always @(op1_addr or destination_reg_addr or reg_wr_en or load_true)

	begin
	if ((op1_addr == destination_reg_addr) && (reg_wr_en == 1'b1) && (load_true == 1'b0))
		bypass_op1_dcd_stage <= 1'b1;
	else
		bypass_op1_dcd_stage <= 1'b0;
	end

always @(op2_addr or destination_reg_addr or reg_wr_en or load_true)

	begin
	if ((op2_addr == destination_reg_addr) && (reg_wr_en == 1'b1) && (load_true == 1'b0))
		bypass_op2_dcd_stage <= 1'b1;
	else
		bypass_op2_dcd_stage <= 1'b0;
	end

assign op1_data = bypass_op1_dcd_stage  ? datamem_wr_data : op1_rd_data;
assign op2_data = bypass_op2_dcd_stage  ? datamem_wr_data : op2_rd_data;


//**********************************************************
//	INSTRUCTION DECODE AND OPERAND FETCH PIPELINE REGISTERS
//***********************************************************

	always @(posedge clk)
		begin
		add_op_true_reg	<= #1	add_op_true;
		or_op_true_reg	<= #1	or_op_true;
		not_op_true_reg	<= #1	not_op_true;
		and_bitwise_true_reg <= #1 and_bitwise_true; 
		or_bitwise_true_reg <= #1 or_bitwise_true;
		not_bitwise_true_reg <= #1 not_bitwise_true;  
		and_op_true_reg	<= #1	and_op_true;
		or_op_true_reg	<= #1	or_op_true;      
		not_op_true_reg	<= #1	not_op_true;
		carry_in_reg	<= #1  carry_in;
		en_op2_complement_reg  <= #1 en_op2_complement;
		nxt_prog_ctr_reg 	<= #1 nxt_prog_ctr;
		jump_true_reg	<= #1 jump_true;
		compare_true_reg <= #1 compare_true;
		op1_data_reg	<= #1 op1_data ;
		op2_data_reg	<= #1 op2_data ;
		shift_left_true_reg <= #1 shift_left_true;
		lgcl_or_bitwse_T_reg <= #1 lgcl_or_bitwse_T;
		store_true_reg <= #1 store_true;
		load_true_reg <= #1 load_true;
		write_to_regfile_reg <= #1 write_to_regfile;
		ld_mem_addr_reg <= #1 ld_mem_addr;
		st_mem_addr_reg <= #1 st_mem_addr;
		invalidate_fetch_instr_r1 <= #1 invalidate_fetch_instr;
		jump_gt_reg <= #1 jump_gt;
		jump_lt_reg <= #1 jump_lt;
		jump_eq_reg <= #1 jump_eq;
		jump_carry_reg <= #1 jump_carry;
		unconditional_jump_reg <= #1 unconditional_jump;
		end

always @(posedge clk)
	if (reset == 1'b1)
		begin
		op1_addr_reg <= 3'b000;
		op2_addr_reg <= 3'b000;
		res_addr_reg <= 3'b000;
		invalidate_decode_instr <= 1'b0;
		end
	else
		begin 
		op1_addr_reg <= #1 op1_addr;          
		op2_addr_reg <= #1 op2_addr;
		res_addr_reg <= #1 res_addr;                
		if (branch_taken_reg == 1'b1)
			invalidate_decode_instr <= #1 1'b1;
		else
			invalidate_decode_instr <= #1 1'b0;	
		end	




//*********************
//EXECUTION UNIT LOGIC
//*********************

//BYPASS logic
//check store pipeline stage signals and address to determine
// if one of the operands have to be bypassed

always @(op1_addr_reg or destination_reg_addr or reg_wr_en or op2_addr_reg or load_true_reg)

	begin
	if ((op1_addr_reg == destination_reg_addr) && (reg_wr_en == 1'b1) && (load_true_reg == 1'b0))
		bypass_op1_ex_stage <= 1'b1;
	else
		bypass_op1_ex_stage <= 1'b0;

	if ((op2_addr_reg == destination_reg_addr) && (reg_wr_en == 1'b1) && (load_true_reg == 1'b0))
		bypass_op2_ex_stage <= 1'b1;
	else
		bypass_op2_ex_stage <= 1'b0;
	end



assign operand1 = bypass_op1_ex_stage  ? datamem_wr_data : op1_data_reg;
assign operand2 = bypass_op2_ex_stage  ? datamem_wr_data : op2_data_reg;

  
// add zero to operand 1 to pass data to store results stage
// in STORE instructions

always @(operand1 or operand2 or en_op2_complement_reg or store_true_reg or add_op_true_reg or lgcl_or_bitwse_T_reg or shift_left_true_reg)

		begin
		data_in1 <= operand1;            

		if (store_true_reg == 1'b1)
		   data_in2 <= 8'b0;
		else if (en_op2_complement_reg == 1)
		   data_in2 <= ~operand2;
		else
		   data_in2 <= operand2;
		end


//Instruction execution

//ALU and store instructionss
always @(data_in1 or data_in2 or carry_in_reg)
		{alu_carry_out, alu_data_out} <= data_in1 + data_in2
							  + carry_in_reg;

// Compare instruction

assign gt_flag_ex = (alu_carry_out == 1'b1) && (alu_data_out != 8'b0) && (compare_true_reg == 1'b1);

assign lt_flag_ex = (alu_carry_out == 1'b0) && (alu_data_out != 8'b0) && (compare_true_reg == 1'b1);

assign eq_flag_ex = (alu_data_out == 8'b00) && (compare_true_reg == 1'b1);

//Shift Left
always @(data_in1)
	begin
	shift_carry_out <= data_in1[7];
	shift_data_out	<= {data_in1[6:0], 1'b0};
	end 

// Logical and bitwiseinstructions                        

always @(and_op_true_reg or or_op_true_reg or not_op_true_reg or and_bitwise_true_reg or or_bitwise_true_reg or not_bitwise_true_reg or data_in1 or data_in2 )

	if (and_op_true_reg == 1'b1)
		logical_data_out <= data_in1 && data_in2;
	else if (or_op_true_reg == 1'b1)
		logical_data_out <= data_in1 || data_in2;       
	else if (not_op_true_reg == 1'b1)
		logical_data_out <= ! data_in1;
	else if (and_bitwise_true_reg == 1'b1)
		logical_data_out <= data_in1 & data_in2;
	else if (or_bitwise_true_reg == 1'b1)
		logical_data_out <= data_in1 | data_in2;
	else logical_data_out <= ! data_in1;   //defaault is NOT op


//merge results

assign data_out = (add_op_true_reg || store_true_reg) ? alu_data_out :
 (lgcl_or_bitwse_T_reg ? logical_data_out : shift_data_out);
                                                         
assign carry_out = add_op_true_reg ? alu_carry_out : shift_carry_out;

assign save_carry_flag = (add_op_true_reg && !compare_true_reg) || shift_left_true_reg;	

// BRANCH LOGIC
// add for conditional jumps

assign gt_flag_true = 
		(((compare_true_r2 && !invalidate_instr) == 1'b1) &&
			gt_flag_ex_reg) || gt_flag;
assign lt_flag_true =
		(((compare_true_r2 && !invalidate_instr) == 1'b1) &&
			lt_flag_ex_reg) || lt_flag;	
assign eq_flag_true =
		(((compare_true_r2 && !invalidate_instr) == 1'b1) &&
			eq_flag_ex_reg) || eq_flag;
	
assign carry_flag_true =
		(((save_carry_flag_reg  && !invalidate_instr) == 1'b1)
		&& carry_flag_ex) || carry_flag ;


assign branch_taken = unconditional_jump_reg || (gt_flag_true && jump_gt_reg) ||
		(lt_flag_true && jump_lt_reg) || (eq_flag_true && jump_eq_reg)  || (carry_flag_true && jump_carry_reg) ;
 

                                              
//****************************************
// EXECUTION STAGE PIPELINE REGISTERS
//****************************************              
                                                        
// Note that asynchronous read of memory ensures that no bypass
// is needed for STORE followed by LOAD

always @(posedge clk)
	begin
	if (save_carry_flag == 1'b1)
		carry_flag_ex <= #1 carry_out;
	operation_result <= #1 data_out;
	data_wr_addr <= #1 store_true_reg ? st_mem_addr_reg : ld_mem_addr_reg; 
	data_rd_addr <= #1 ld_mem_addr_reg; 
	data_out_reg <= #1 data_out;
	load_mem_data <= #1 load_true_reg;
	invalidate_fetch_instr_r2 <= #1 invalidate_fetch_instr_r1;
	invalidate_decode_instr_r1 <= #1 invalidate_decode_instr; 
	gt_flag_ex_reg <= #1 gt_flag_ex;
	lt_flag_ex_reg <= #1 lt_flag_ex;
	eq_flag_ex_reg <= #1 eq_flag_ex;
	compare_true_r2 <= #1 compare_true_reg;       
	nxt_prog_ctr_r2 <= #1 nxt_prog_ctr_reg;
	end
// data_wr_addr[10:8] is also the result wr addr
// and datamem wr data is also the wr data for register writes

//Disable register and memory writes during reset
always @(posedge clk)
	if (reset == 1'b1)
	   begin
		store_to_mem_ex <= #1 1'b0;
		reg_wr_en_ex <= #1 1'b0;
		destination_reg_addr <= 3'b0;
		branch_taken_reg <= 1'b0;
		invalidate_execute_instr <= 1'b0;
		save_carry_flag_reg <= 1'b0;
	   end
	else
	   begin
		store_to_mem_ex <= #1 store_true_reg;
		reg_wr_en_ex <= #1 write_to_regfile_reg;
		destination_reg_addr <= #1 res_addr_reg;
		branch_taken_reg <= #1 branch_taken;
		save_carry_flag_reg <= #1 save_carry_flag; 
		if (branch_taken_reg == 1'b1)
			invalidate_execute_instr <= #1 1'b1;
		else
			invalidate_execute_instr <= #1 1'b0;
	   end


//****************************************
// STORE RESULTS STAGE LOGIC
//****************************************

//During LOAD instruction data is fetched from memory and
// written to the register here

assign datamem_wr_data = load_mem_data ? datamem_rd_data : data_out_reg;

assign invalidate_instr = (invalidate_fetch_instr_r2 ||
	invalidate_decode_instr_r1 || invalidate_execute_instr);

assign  store_to_mem = (store_to_mem_ex && !invalidate_instr);

assign reg_wr_en = (reg_wr_en_ex && !invalidate_instr);



//****************************************
// STORE RESULT STAGE PIPELINE REGISTERS
//****************************************
always @(posedge clk)
	begin                                
	if (reset == 1'b1)                     
		begin
		carry_flag <= 1'b0;
		gt_flag <= 1'b0;
		lt_flag <= 1'b0;
		eq_flag <= 1'b0;
		end
		
	else 
		begin
		if ((save_carry_flag_reg && !invalidate_instr) == 1'b1)
	carry_flag <= #1 carry_flag_ex;

		if ((compare_true_r2 && !invalidate_instr) == 1'b1)
			begin
			gt_flag <= #1 gt_flag_ex_reg;
			lt_flag <= #1 lt_flag_ex_reg;
			eq_flag <= #1 eq_flag_ex_reg;
			end
		end
	end


//****************************************
// PROGRAM COUNTER
//****************************************

always @(posedge clk)
	if (reset == 1'b1)
	   prog_ctr <= #1 10'b1;
	else
	   begin
		if (branch_taken_reg == 1) //update in store res stage
			begin                             
			prog_ctr <= #1 nxt_prog_ctr_r2;
			set_invalidate_instruction <= #1 1'b1;
			end
		else
			begin
			prog_ctr <= #1 prog_ctr + 1'b1;
			set_invalidate_instruction <= #1 1'b0;
			end

	   end

endmodule


module register_file (clk, reset, wr_data, rd_data1, rd_data2, rd_addr1, rd_addr2, wr_addr, wr_en);

	input clk, reset;
	input [7:0] wr_data;
	input [2:0] rd_addr1, rd_addr2, wr_addr;
	input		 wr_en;
	output[7:0] rd_data1, rd_data2;

	reg [7:0] rd_data1, rd_data2;


//	register file
	reg [7:0] reg_file [7:0];
                                         	
	always @(rd_addr1 or rd_addr2 or reset or wr_en or wr_data)
		begin
		rd_data1 <= reg_file[rd_addr1];
		rd_data2 <= reg_file[rd_addr2];
		end

always @(posedge clk)	
		begin
		if (wr_en == 1)
			reg_file[wr_addr] <= #1 wr_data;
		end

endmodule