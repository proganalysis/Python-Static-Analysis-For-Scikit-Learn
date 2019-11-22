package master;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

import com.ibm.wala.cast.ir.ssa.AstLexicalAccess;
import com.ibm.wala.cfg.Util;
import com.ibm.wala.shrikeBT.IBinaryOpInstruction;
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction;
import com.ibm.wala.ssa.IR;
import com.ibm.wala.ssa.ISSABasicBlock;
import com.ibm.wala.ssa.SSABinaryOpInstruction;
import com.ibm.wala.ssa.SSACFG;
import com.ibm.wala.ssa.SSAConditionalBranchInstruction;
import com.ibm.wala.ssa.SSAGotoInstruction;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAPhiInstruction;
import com.ibm.wala.ssa.SSAThrowInstruction;
import com.ibm.wala.ssa.SSACFG.ExceptionHandlerBasicBlock;
import com.ibm.wala.util.collections.Iterator2Iterable;

import master.Constraint.bool_operator;
import master.MathConstraint.bool_math_operator;
import master.MathNumber.ari_math_operator;

public class PreconditionFinder {
	private HashMap<ISSABasicBlock, Constraint> block_to_constraint;
	public PreconditionFinder() {
		block_to_constraint = new HashMap<ISSABasicBlock, Constraint>();
	}
	public String findWeakest(IR ir) {
		Iterator<ISSABasicBlock> itr = ir.getBlocks();
		SSAInstruction[] instructions = ir.getInstructions();
		CustomVisitor vis = new CustomVisitor();
		while (itr.hasNext()) {
			ISSABasicBlock current_block = itr.next();
	        // System.err.println("AAAAA" + current_block); 
	        int start = current_block.getFirstInstructionIndex();
	        int last = current_block.getLastInstructionIndex();
	        for (int i = start; i <= last; i++) {
	        	if(instructions[i] != null) {
	        		boolean is_a_throw = false;
	        		if (instructions[i] instanceof SSAThrowInstruction) {
	        			is_a_throw = true;
	        		}
	        		
	        		if (is_a_throw) {
	        			FindPre(current_block, i, ir);
	        		}
	        		
	        	} else {
	        		//System.err.println(i + " is null");
	        	}
	        }
		}
		
		itr = ir.getBlocks(); 
		System.err.println("PRES");
		while (itr.hasNext()) {
			ISSABasicBlock current_block = itr.next();
			System.err.println(current_block);
			if (block_to_constraint.containsKey(current_block)) {
				System.err.println(block_to_constraint.get(current_block));
			}
		}
			
		return "";
	}
	
	private boolean DoReplacement(ISSABasicBlock last_block, ISSABasicBlock current_block, IR ir, boolean first) {
		
		int start = current_block.getFirstInstructionIndex();
        int last = current_block.getLastInstructionIndex();
        SSAInstruction[] instructions = ir.getInstructions();
        CustomVisitor vis = new CustomVisitor();
        System.err.println("running");
        System.err.println(last_block);
        System.err.println(current_block);
        // Check if last block had a phi.
        // Convert anything in phi to the ones we want.
        /*
        boolean has_phi = false;
        SSAPhiInstruction phi2 = null;
        for (SSAPhiInstruction phi : Iterator2Iterable.make(last_block.iteratePhis())) {
            if (phi != null) {
            	//int phi_left = phi.getDef();
            	//int num_use = phi.getNumberOfUses();
            	//HashSet<Integer> uses = new HashSet<Integer>();
            	//for (int i = 0; i < phi.getNumberOfUses(); i++) {
            	//	uses.add(phi.getUse(i));
            	//}
            	has_phi = true;
            	phi2 = phi;
            }
        }
        */
        
        // Last block has a phi so we're in an if else
        /*
        if(has_phi) {
        	boolean is_goto = false;
        	boolean is_if = false;
        	if (instructions[last] != null) {
        		instructions[last].visit(vis);
        		is_if = vis.getIf();
        	}
        	if (instructions[last] != null) {
        		instructions[last].visit(vis);
        		is_goto = vis.GetIsGoto();
        	}
        	
        	// Only if the last isn't an if
        	if(!is_if) {
        		if(is_goto) {
        			// check if its an if or an if else
        			System.err.println("Checking");
        			SSAGotoInstruction go_to_instruct = (SSAGotoInstruction)instructions[last];
        			System.err.println(go_to_instruct.getTarget());
        			// one if
        			if(go_to_instruct.getTarget() - 1 == go_to_instruct.iindex) {
        				int phi_left = phi2.getDef();
                		int phi_right = phi2.getUse(1);
                		c.FindAndReplaceVar(phi_left, phi_right);
        			} else { // if part of an if else
        				int phi_left = phi2.getDef();
                		int phi_right = phi2.getUse(0);
                		c.FindAndReplaceVar(phi_left, phi_right);
        			}
            		
            	} else { // no goto so its else
            		System.err.println("else");
            		int phi_left = phi2.getDef();
            		int phi_right = phi2.getUse(1);
            		System.err.println(phi_left);
            		System.err.println(phi_right);
            		System.err.println(c);
            		c.FindAndReplaceVar(phi_left, phi_right);
            		//System.err.println(current_block);
            		System.err.println(c);
            	}
        	}
        }
        */
        Constraint c = null;
		
        // Check if the last block is an if.
        if (!first && current_block.getLastInstructionIndex() >= 0 && Util.endsWithConditionalBranch(ir.getControlFlowGraph(), current_block)) {
        	ISSABasicBlock block1;
        	ISSABasicBlock block2;
        	block1 = Util.getNotTakenSuccessor(ir.getControlFlowGraph(), current_block);
        	block2 = Util.getTakenSuccessor(ir.getControlFlowGraph(), current_block);
        	if(block_to_constraint.containsKey(block1) == false || block_to_constraint.containsKey(block2) == false) {
        		return false;
        	}
        	Constraint c1 = Constraint.getCopy(block_to_constraint.get(block1));
        	System.err.println(block2);
        	Constraint c2 = Constraint.getCopy(block_to_constraint.get(block2));
        	int param_one = -1;
    		int param_two = -1;
    		String var_name1 = "";
    		String var_name2 = "";
    		String if_name = "";
    		bool_math_operator link = bool_math_operator.EQ;
    		bool_math_operator rlink = bool_math_operator.NEQ;
			SSACFG cfg = ir.getControlFlowGraph();
			int vn = current_block.getLastInstruction().getUse(0);
			boolean found_invoke = false;
			Iterator<ISSABasicBlock> itr2 = cfg.getPredNodes(current_block);
			
			// Find what made up the if
			int start2 = current_block.getFirstInstructionIndex();
	        int last2 = current_block.getLastInstructionIndex() - 1;
	        for (int j = last2; j >= start2; j--) {
	        	if(instructions[j] != null) {
	        		if(instructions[j] instanceof SSABinaryOpInstruction) {
        				param_one = instructions[j].getUse(0);
        				param_two = instructions[j].getUse(1);
        				String[] t1 = ir.getLocalNames(j, param_one);
        				String[] t2 = ir.getLocalNames(j, param_two);
        				if(t1.length >= 1) {
        					var_name1 = ir.getLocalNames(j, param_one)[0];
        				}
        				if(t2.length >= 1) {
        					var_name2 = ir.getLocalNames(j, param_two)[0];
        				}
        				
        				SSABinaryOpInstruction temp = (SSABinaryOpInstruction)instructions[j];
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.EQ.toString())) {
        					link = bool_math_operator.NEQ;
        					rlink = bool_math_operator.EQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.NE.toString())) {
        					link = bool_math_operator.EQ;
        					rlink = bool_math_operator.NEQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.LT.toString())) {
        					link = bool_math_operator.GEQ;
        					rlink = bool_math_operator.L;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.GT.toString())) {
        					link = bool_math_operator.LEQ;
        					rlink = bool_math_operator.G;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.LE.toString())) {
        					link = bool_math_operator.G;
        					rlink = bool_math_operator.LEQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.GE.toString())) {
        					link = bool_math_operator.L;
        					rlink = bool_math_operator.GEQ;
        				}
	        		}
	        	}
	        }
			
			MathNumber right = new MathNumber();
			MathNumber left = new MathNumber();
			left.setVariable(param_one, var_name1);
			right.setVariable(param_two, var_name2);
			Constraint temp1 = new MathConstraint(left, right, link);
			c1 = new Constraint(temp1, c1, bool_operator.OR);
			Constraint temp2 = new MathConstraint(left, right, rlink);
			c2 = new Constraint(temp2, c2, bool_operator.OR);
			c = new Constraint(c1, c2, bool_operator.AND);
			MathNumber mn1 = new MathNumber();
			mn1.setVariable(param_one, var_name1);
			c.FindAndReplace(param_one, mn1);
			MathNumber mn2 = new MathNumber();
			mn2.setVariable(param_two, var_name2);
			c.FindAndReplace(param_two, mn2);
			
        } else {
        	c = Constraint.getCopy(block_to_constraint.get(last_block));
        }
        
        for (int i = last; i >= start; i--) { 
        	if(instructions[i] != null) {
        		boolean is_a_binary = false;
        		if (instructions[i] instanceof SSABinaryOpInstruction) {
        			is_a_binary = true;
        		}
        		if (is_a_binary) {
        			int result = instructions[i].getDef();
    				//String result = ir.getLocalNames(i, instructions[i].getDef())[0];
    				int param_one = instructions[i].getUse(0);
    				int param_two = instructions[i].getUse(1);
    				String var_name1 = "";
    				String var_name2 = "";
    				if ((int) ir.getLocalNames(i, param_one).length > 0) {
    					var_name1 = ir.getLocalNames(i, param_one)[0];
    				}
    				if ((int) ir.getLocalNames(i, param_two).length > 0) {
        				var_name2 = ir.getLocalNames(i, param_two)[0];
    				}
        			if (((SSABinaryOpInstruction) instructions[i]).getOperator().equals(IBinaryOpInstruction.Operator.ADD)) {
        				MathNumber mn = new MathNumber(param_one, param_two, var_name1, var_name2, ari_math_operator.ADD);
        				if (ir.getSymbolTable().isConstant(param_one)) {
        					if(ir.getSymbolTable().isIntegerConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isIntegerConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getIntValue(param_two));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getStringValue(param_two));
        					}
        				}
        				c.FindAndReplace(result, mn);
        			}
        			if (((SSABinaryOpInstruction) instructions[i]).getOperator().equals(IBinaryOpInstruction.Operator.MUL)) {
        				MathNumber mn = new MathNumber(param_one, param_two, var_name1, var_name2, ari_math_operator.MULT);
        				if (ir.getSymbolTable().isConstant(param_one)) {
        					if(ir.getSymbolTable().isIntegerConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isIntegerConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getIntValue(param_two));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getStringValue(param_two));
        					}
        				}
        				c.FindAndReplace(result, mn);
        			}
        		}
        	}
        }
        
        boolean has_phi = false;
        SSAPhiInstruction phi2 = null;
        for (SSAPhiInstruction phi : Iterator2Iterable.make(current_block.iteratePhis())) {
            if (phi != null) {
            	has_phi = true;
            	phi2 = phi;
            	int phi_left = phi2.getDef();
        		int phi_right1 = phi2.getUse(0);
        		int phi_right2 = phi2.getUse(1);
        		String var_name1 = "";
    			String var_name2 = "";
    			if ((int) ir.getLocalNames(current_block.getFirstInstructionIndex(), phi_right1).length > 0) {
    				var_name1 = ir.getLocalNames(current_block.getFirstInstructionIndex(), phi_right1)[0];
    			}
    			if ((int) ir.getLocalNames(current_block.getFirstInstructionIndex(), phi_right2).length > 0) {
    				var_name2 = ir.getLocalNames(current_block.getFirstInstructionIndex(), phi_right2)[0];
    			}
        		MathNumber mn = new MathNumber();
        		for(int i = 0; i < phi2.getNumberOfUses(); i++) {
        			mn.addToPhi(phi2.getUse(i));
        		}
    			c.FindAndReplace(phi_left, mn);
            }
        }
        
        block_to_constraint.put(current_block, c);
        return true;
	}
	
	private void DoReplacement(ISSABasicBlock current_block, IR ir) {
		int start = current_block.getFirstInstructionIndex();
        int last = current_block.getLastInstructionIndex();
        SSAInstruction[] instructions = ir.getInstructions();
        Constraint c = block_to_constraint.get(current_block);
        boolean ignore_first_if = true;
        Queue<ISSABasicBlock> queue = new LinkedList<>(); 
        queue.add(current_block);
        System.err.println("first");
        while (!queue.isEmpty()) {
        	ISSABasicBlock last_block = queue.poll();
        	Iterator<ISSABasicBlock> itr = ir.getControlFlowGraph().getPredNodes(last_block);
        	while(itr.hasNext()) {
        		ISSABasicBlock next_block = itr.next();
        		System.err.println(next_block);
        		if (DoReplacement(last_block, next_block, ir, ignore_first_if) ) {
        			queue.add(next_block);
        			ignore_first_if = false;
        		}
        		
        	}
        }
        
	}
	
	//look at ocelot and z3
	private Constraint GetPreFor(ISSABasicBlock last_block, IR ir) {
		SSAInstruction[] instructions = ir.getInstructions();
		
		Iterator<ISSABasicBlock> itr3 = ir.getControlFlowGraph().getPredNodes(last_block);
		if (!itr3.hasNext()) {
			return null;
		}
		ISSABasicBlock current_block = itr3.next();
		if (itr3.hasNext()) {
			return null;
		}
		if (current_block.getLastInstructionIndex() < 0) {
			return null;
		}
		SSAInstruction last_instruction = current_block.getLastInstruction();
		boolean is_a_if = false;
		if (last_instruction instanceof SSAConditionalBranchInstruction) {
			is_a_if = true;
		}
		System.err.println(current_block);
		System.err.println("GetPreFor");
		int param_one = -1;
		int param_two = -1;
		String var_name1 = "";
		String var_name2 = "";
		String if_name = "";
		bool_math_operator link = bool_math_operator.EQ;
		if (is_a_if) {
			SSACFG cfg = ir.getControlFlowGraph();
			System.err.println("IFS");
			System.err.println(last_instruction.toString(ir.getSymbolTable()));
			int vn = last_instruction.getUse(0);
			System.err.println(vn);
			boolean found_invoke = false;
			Iterator<ISSABasicBlock> itr2 = cfg.getPredNodes(current_block);
			
			// Find what made up the if
			int start2 = current_block.getFirstInstructionIndex();
	        int last2 = current_block.getLastInstructionIndex() - 1;
	        for (int j = last2; j >= start2; j--) {
	        	if(instructions[j] != null) {
	        		
	        		if(instructions[j] instanceof SSABinaryOpInstruction) {
        				param_one = instructions[j].getUse(0);
        				param_two = instructions[j].getUse(1);
        				String[] t1 = ir.getLocalNames(j, param_one);
        				String[] t2 = ir.getLocalNames(j, param_two);
        				if(t1.length >= 1) {
        					var_name1 = ir.getLocalNames(j, param_one)[0];
        				}
        				if(t2.length >= 1) {
        					var_name2 = ir.getLocalNames(j, param_two)[0];
        				}
        				
        				SSABinaryOpInstruction temp = (SSABinaryOpInstruction)instructions[j];
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.EQ.toString())) {
        					link = bool_math_operator.NEQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.NE.toString())) {
        					link = bool_math_operator.EQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.LT.toString())) {
        					link = bool_math_operator.GEQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.GT.toString())) {
        					link = bool_math_operator.LEQ;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.LE.toString())) {
        					link = bool_math_operator.G;
        				}
        				if (temp.getOperator().toString().equals(IConditionalBranchInstruction.Operator.GE.toString())) {
        					link = bool_math_operator.L;
        				}
        				break;
	        		}
	        	}
	        }
		}
		
		Constraint c;
		MathNumber right = new MathNumber();
		MathNumber left = new MathNumber();
		left.setVariable(param_one, var_name1);
		right.setVariable(param_two, var_name2);
		if (ir.getSymbolTable().isConstant(param_one)) {
			if(ir.getSymbolTable().isIntegerConstant(param_one)) {
				left.setConstant(ir.getSymbolTable().getIntValue(param_one));
			}
			if(ir.getSymbolTable().isStringConstant(param_one)) {
				left.setConstant(ir.getSymbolTable().getStringValue(param_one));
			}
		}
		if (ir.getSymbolTable().isConstant(param_two)) {
			if(ir.getSymbolTable().isIntegerConstant(param_two)) {
				right.setConstant(ir.getSymbolTable().getIntValue(param_two));
			}
			if(ir.getSymbolTable().isStringConstant(param_two)) {
				right.setConstant(ir.getSymbolTable().getStringValue(param_two));
			}
		}
		c = new MathConstraint(left, right, link);
		
		if (!block_to_constraint.containsKey(last_block)) {
			block_to_constraint.put(last_block, c);
			block_to_constraint.put(current_block, c);
		} else {
			Constraint c2 = block_to_constraint.get(last_block);
			Constraint c3 = new Constraint(c, c2, bool_operator.AND);
			block_to_constraint.put(last_block, c3);
		}
		
		// Do replace assignments
		DoReplacement(last_block, ir);
		
		return null;
		
	}
	
	
	// Minature, WALA menSat, visit Binary Operation
	private void FindPre(ISSABasicBlock current_block, int index, IR ir) {
		System.err.println("start");
		SSAInstruction[] instructions = ir.getInstructions();
		int start = current_block.getFirstInstructionIndex();
        int last = current_block.getLastInstructionIndex();
        SSACFG cfg = ir.getControlFlowGraph();
        Iterator<ISSABasicBlock> itr2 = cfg.getPredNodes(current_block);
        Queue<ISSABasicBlock> queue = new LinkedList<>(); 
        GetPreFor(current_block, ir);
		queue.add(current_block);
		// Constraint c = GetPreFor(null, prev_block, ir);
        while (!queue.isEmpty()) {
        	ISSABasicBlock current_block2 = queue.poll();
        	//GetPreFor(current_block2, ir);
        	Iterator<ISSABasicBlock> itr3 = cfg.getPredNodes(current_block2);
        	while(itr3.hasNext()) {
        		ISSABasicBlock temp_block = itr3.next();
        		queue.add(temp_block);
        		Constraint c = block_to_constraint.get(current_block2);
        	}
        }
        
        for (int i = start; i <= last; i++) {
        	if(instructions[i] != null) {
        		System.err.println(instructions[i].toString(ir.getSymbolTable()));
        	}
        }
	}
}
