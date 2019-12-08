package master;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

import com.ibm.wala.cast.ir.ssa.AstLexicalRead;
import com.ibm.wala.cast.python.ssa.PythonInvokeInstruction;
import com.ibm.wala.cfg.Util;
import com.ibm.wala.shrikeBT.IBinaryOpInstruction;
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction;
import com.ibm.wala.ssa.IR;
import com.ibm.wala.ssa.ISSABasicBlock;
import com.ibm.wala.ssa.SSABinaryOpInstruction;
import com.ibm.wala.ssa.SSAConditionalBranchInstruction;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAPhiInstruction;
import com.ibm.wala.ssa.SSAThrowInstruction;
import com.ibm.wala.util.collections.Iterator2Iterable;

import master.Constraint.bool_operator;
import master.MathConstraint.bool_math_operator;
import master.MathNumber.ari_math_operator;

public class PreconditionFinder {
	private HashMap<ISSABasicBlock, Constraint> block_to_constraint;
	private HashMap<String, Constraint> method_name_to_constraint;
	private HashMap<String, ArrayList<Integer>> method_name_to_params;
	private boolean redo;
	public PreconditionFinder() {
		block_to_constraint = new HashMap<ISSABasicBlock, Constraint>();
		method_name_to_constraint = new HashMap<String, Constraint>();
		method_name_to_params = new HashMap<String, ArrayList<Integer>>();
		redo = true;
	}
	
	public boolean GetRedo() {
		return redo;
	}
	
	public void setRedo(boolean t) {
		redo = t;
	}
	
	public boolean findWeakest(IR ir, String current_method, HashSet<Integer> param_set, ArrayList<Integer> param_arr) {
		Iterator<ISSABasicBlock> itr = ir.getBlocks();
		SSAInstruction[] instructions = ir.getInstructions();
		boolean found_throw = false;
		while (itr.hasNext()) {
			ISSABasicBlock current_block = itr.next();
	        int start = current_block.getFirstInstructionIndex();
	        int last = current_block.getLastInstructionIndex();
	        for (int i = start; i <= last; i++) {
	        	if(instructions[i] != null) {
	        		// We found the throw.
	        		if (instructions[i] instanceof SSAThrowInstruction) {
	        			found_throw = true;
	        			FindPre(current_block, i, ir);
	        		}
	        		
	        		// We found a function invoke, check if the previous line is the function def.
	        		if (instructions[i] instanceof PythonInvokeInstruction) {
	        			int j = i - 1;
	        			if (instructions[j] instanceof AstLexicalRead) {
	        				AstLexicalRead temp = (AstLexicalRead)instructions[j];
	        				
	        				// Function name
		        			String key = temp.getAccess(0).variableName;
		        			// See if this function's precondition is calculated.
		        			if (method_name_to_constraint.containsKey(key)) {
		        				// We need to map what variables we use to the function parameters.
		        				HashMap<Integer, Integer> map_param_to_current = new HashMap<Integer, Integer>();
		        				ArrayList<Integer> function_params = method_name_to_params.get(key);
		        				for (int k = 0; k < function_params.size(); k++) {
		        					map_param_to_current.put(function_params.get(k), instructions[i].getUse(k+1));
		        				}
		        				// Add the function's precondition to our current block.
		        				block_to_constraint.put(current_block, new Constraint(method_name_to_constraint.get(key)));
		        				block_to_constraint.get(current_block).reconfigure(map_param_to_current, ir, i);
		        				DoReplacement(current_block, ir, true);
		        				// We consider this as finding a throw.
		        				found_throw = true;
		        			}
	        			}
	        		}
	        	}
	        }
		}
		
		itr = ir.getBlocks(); 
		System.err.println("What's the current function's blocks's preconditions.");
		int count = 0;
		while (itr.hasNext()) {
			ISSABasicBlock current_block = itr.next();
			System.err.println(current_block);
			
			// The second block is effectively the first block since it starts with the first non-negative index.
			// If we have the second block done save the precondition in there for the function.
			if (block_to_constraint.containsKey(current_block)) {
				System.err.println(block_to_constraint.get(current_block));
				if (count == 1) {
					method_name_to_constraint.put(current_method, block_to_constraint.get(current_block));
					method_name_to_params.put(current_method, param_arr);
				}
				count++;
			}
		}
		if (found_throw) {
			// We lock the function so future functions can't accidently replace the variables.
			method_name_to_constraint.get(current_method).lock(param_set, current_method);
			System.err.println("Calculated the precondition.");
			System.err.println(method_name_to_constraint.get(current_method));
			redo = true;
		}
		return found_throw;
	}
	
	private boolean DoReplacement(ISSABasicBlock last_block, ISSABasicBlock current_block, IR ir, boolean first) {
		
		int start = current_block.getFirstInstructionIndex();
        int last = current_block.getLastInstructionIndex();
        SSAInstruction[] instructions = ir.getInstructions();
        Constraint c = null;
		
        // Check if the last block is an if. We do special logic if it is an if.
        if (!first && current_block.getLastInstructionIndex() >= 0 && Util.endsWithConditionalBranch(ir.getControlFlowGraph(), current_block)) {
        	ISSABasicBlock block1;
        	ISSABasicBlock block2;
        	block1 = Util.getNotTakenSuccessor(ir.getControlFlowGraph(), current_block);
        	block2 = Util.getTakenSuccessor(ir.getControlFlowGraph(), current_block);
        	if(block_to_constraint.containsKey(block1) == false || block_to_constraint.containsKey(block2) == false) {
        		return false;
        	}
        	Constraint c1 = Constraint.getCopy(block_to_constraint.get(block1));
        	Constraint c2 = Constraint.getCopy(block_to_constraint.get(block2));
        	int param_one = -1;
    		int param_two = -1;
    		String var_name1 = "";
    		String var_name2 = "";
    		bool_math_operator link = bool_math_operator.EQ;
    		bool_math_operator rlink = bool_math_operator.NEQ;
			
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
        				break;
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
			if (ir.getSymbolTable().isConstant(param_one)) {
				if(ir.getSymbolTable().isNumberConstant(param_one)) {
					mn1.setConstant(ir.getSymbolTable().getIntValue(param_one));
				}
				if(ir.getSymbolTable().isStringConstant(param_one)) {
					mn1.setConstant(ir.getSymbolTable().getStringValue(param_one));
				}
			}
			c.FindAndReplace(param_one, mn1);
			MathNumber mn2 = new MathNumber();
			mn2.setVariable(param_two, var_name2);
			if (ir.getSymbolTable().isConstant(param_two)) {
				
				if(ir.getSymbolTable().isNumberConstant(param_two)) {
					mn2.setConstant(ir.getSymbolTable().getIntValue(param_two));
				}
				if(ir.getSymbolTable().isStringConstant(param_two)) {
					mn2.setConstant(ir.getSymbolTable().getStringValue(param_two));
				}
			}
			c.FindAndReplace(param_two, mn2);
        } else {
        	c = Constraint.getCopy(block_to_constraint.get(last_block));
        }
        
        // Go through the instructions and replace.
        for (int i = last; i >= start; i--) { 
        	if(instructions[i] != null) {
        		boolean is_a_binary = false;
        		if (instructions[i] instanceof SSABinaryOpInstruction) {
        			is_a_binary = true;
        		}
        		if (is_a_binary) {
        			int result = instructions[i].getDef();
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
        					if(ir.getSymbolTable().isNumberConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isNumberConstant(param_two)) {
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
        					if(ir.getSymbolTable().isNumberConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isNumberConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getIntValue(param_two));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getStringValue(param_two));
        					}
        				}
        				c.FindAndReplace(result, mn);
        			}
        			if (((SSABinaryOpInstruction) instructions[i]).getOperator().equals(IBinaryOpInstruction.Operator.DIV)) {
        				MathNumber mn = new MathNumber(param_one, param_two, var_name1, var_name2, ari_math_operator.DIV);
        				if (ir.getSymbolTable().isConstant(param_one)) {
        					if(ir.getSymbolTable().isNumberConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isNumberConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getIntValue(param_two));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_two)) {
        						mn.getRight().setConstant(ir.getSymbolTable().getStringValue(param_two));
        					}
        				}
        				c.FindAndReplace(result, mn);
        			}
        			if (((SSABinaryOpInstruction) instructions[i]).getOperator().equals(IBinaryOpInstruction.Operator.SUB)) {
        				MathNumber mn = new MathNumber(param_one, param_two, var_name1, var_name2, ari_math_operator.SUB);
        				if (ir.getSymbolTable().isConstant(param_one)) {
        					if(ir.getSymbolTable().isNumberConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getIntValue(param_one));
        					}
        					if(ir.getSymbolTable().isStringConstant(param_one)) {
        						mn.getLeft().setConstant(ir.getSymbolTable().getStringValue(param_one));
        					}
        				}
        				if (ir.getSymbolTable().isConstant(param_two)) {
        					if(ir.getSymbolTable().isNumberConstant(param_two)) {
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
        
        SSAPhiInstruction phi2 = null;
        for (SSAPhiInstruction phi : Iterator2Iterable.make(current_block.iteratePhis())) {
            if (phi != null) {
            	phi2 = phi;
            	int phi_left = phi2.getDef();
        		MathNumber mn = new MathNumber();
        		for (int i = 0; i < phi2.getNumberOfUses(); i++) {
        			mn.addToPhi(phi2.getUse(i));
        		}
    			c.FindAndReplace(phi_left, mn);
            }
        }
        
        block_to_constraint.put(current_block, c);
        return true;
	}
	
	private void DoReplacement(ISSABasicBlock current_block, IR ir, boolean special) {
		// Special means we called this with a precondition found from another function.
        if (special) {
        	DoReplacement(current_block, current_block, ir, true);
        }
        
        boolean ignore_first_if = true;
        Queue<ISSABasicBlock> queue = new LinkedList<>();
        queue.add(current_block);
        while (!queue.isEmpty()) {
        	ISSABasicBlock last_block = queue.poll();
        	Iterator<ISSABasicBlock> itr = ir.getControlFlowGraph().getPredNodes(last_block);
        	while(itr.hasNext()) {
        		ISSABasicBlock next_block = itr.next();
        		if (DoReplacement(last_block, next_block, ir, ignore_first_if) ) {
        			queue.add(next_block);
        			ignore_first_if = false;
        		}
        	}
        }
	}
	
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
		int param_one = -1;
		int param_two = -1;
		String var_name1 = "";
		String var_name2 = "";
		bool_math_operator link = bool_math_operator.EQ;
		if (is_a_if) {
			
			// Find what made up the if
			int start2 = current_block.getFirstInstructionIndex();
	        int last2 = current_block.getLastInstructionIndex() - 1;
	        for (int j = last2; j >= start2; j--) {
	        	if (instructions[j] != null) {
	        		if (instructions[j] instanceof SSABinaryOpInstruction) {
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
		
		MathNumber right = new MathNumber();
		MathNumber left = new MathNumber();
		left.setVariable(param_one, var_name1);
		right.setVariable(param_two, var_name2);
		if (ir.getSymbolTable().isConstant(param_one)) {
			if (ir.getSymbolTable().isNumberConstant(param_one)) {
				left.setConstant(ir.getSymbolTable().getIntValue(param_one));
			}
			if (ir.getSymbolTable().isStringConstant(param_one)) {
				left.setConstant(ir.getSymbolTable().getStringValue(param_one));
			}
		}
		if (ir.getSymbolTable().isConstant(param_two)) {
			if (ir.getSymbolTable().isNumberConstant(param_two)) {
				right.setConstant(ir.getSymbolTable().getIntValue(param_two));
			}
			if (ir.getSymbolTable().isStringConstant(param_two)) {
				right.setConstant(ir.getSymbolTable().getStringValue(param_two));
			}
		}
		
		// The first constraint found from the throw.
		Constraint c = new MathConstraint(left, right, link);
		
		if (!block_to_constraint.containsKey(last_block)) {
			block_to_constraint.put(last_block, c);
			block_to_constraint.put(current_block, c);
		} 
		
		// Do replace assignments
		DoReplacement(last_block, ir, false);
		
		return null;
	}
	
	private void FindPre(ISSABasicBlock current_block, int index, IR ir) {
		System.err.println("Starting FindPre");
        GetPreFor(current_block, ir);
	}
}
