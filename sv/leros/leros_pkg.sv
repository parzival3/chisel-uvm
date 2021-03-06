//  Package: leros_pkg
//
package leros_pkg;
	import uvm_pkg::*;
	`include "uvm_macros.svh"


	//  Group: Typedefs
	typedef enum logic[2:0] { NOP, ADD, SUB, AND, OR, XOR, LD, SHR } leros_op_t;

	//  Group: Parameters

	//  Group: Helper classes
	class leros_command;
		leros_op_t op;
		logic [15:0] din;
		logic reset;
	endclass

	class sequence_config;
		int no_repeats;
	endclass;


	//  Group: File includes
	`include "base_transaction.svh"
	`include "hilo_transaction.svh"

	typedef uvm_sequencer#(base_transaction) my_sequencer;

	`include "base_sequence.svh"
	`include "reset_sequence.svh"
	`include "ldadd_sequence.svh"
	`include "random_sequence.svh"


	`include "driver.svh"
	`include "command_monitor.svh"
	`include "result_monitor.svh"
	`include "coverage.svh"
	`include "scoreboard.svh"

	`include "env.svh"
	`include "base_test.svh"
	`include "hilo_test.svh"
	
endpackage: leros_pkg
