package master;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import com.ibm.wala.ssa.IR;

public class MathNumber {
	
	enum var_type {
		MATH, ENUM, UNKNOWN;
	}
	
	// If None, nothing is set. Phi is special
	enum ari_math_operator {
		ADD, SUB, MULT, DIV, PHI, NONE;
	}
	
	private String constant_val;
	private int variable;
	private var_type variable_type;
	private String var_name;
	private String method_name;
	private boolean locked;
	private boolean has_constant;
	private boolean has_variable;
	//if has_phi is true, left and right have to be vars
	private boolean has_phi;
	private Set<Integer> phis;
	private MathNumber left;
	private MathNumber right;
	private ari_math_operator oper; 
	MathNumber() {
		constant_val = "";
		variable = 0;
		has_constant = false;
		has_variable = false;
		has_phi = false;
		left = null;
		right = null;
		var_name = "";
		oper = ari_math_operator.NONE;
		phis = new HashSet<Integer>();
		variable_type = var_type.UNKNOWN;
		locked = false;
		method_name = "";
	}
	
	MathNumber(MathNumber mn) {
		constant_val = mn.constant_val;
		variable = mn.variable;
		has_constant = mn.has_constant;
		has_variable = mn.has_variable;
		has_phi = mn.has_phi;
		left = null;
		right = null;
		if (mn.left != null) {
			left = new MathNumber(mn.left);
		}
		if (mn.right != null) {
			right = new MathNumber(mn.right);
		}
		var_name = mn.var_name;
		oper = mn.oper;
		phis = new HashSet<Integer>(mn.phis);
		variable_type = mn.variable_type;
		locked = mn.locked;
		method_name = mn.method_name;
	}
	
	MathNumber(int var1, int var2, String name1, String name2, ari_math_operator op) {
		constant_val = "";
		variable = 0;
		has_constant = false;
		has_variable = false;
		left = new MathNumber();
		right = new MathNumber();
		left.setVariable(var1, name1);
		right.setVariable(var2, name2);
		oper = op;
		var_name = "";
		phis = new HashSet<Integer>();
		variable_type = var_type.UNKNOWN;
		locked = false;
		method_name = "";
	}
	
	public void assign(MathNumber nm) {
		constant_val = nm.constant_val;
		variable = nm.variable;
		has_constant = nm.has_constant;
		has_variable = nm.has_variable;
		left = nm.left;
		right = nm.right;
		oper = nm.oper;
		var_name = nm.var_name;
		phis = nm.phis;
		variable_type = nm.variable_type;
	}
	
	public void setVariable(int i) {
		variable = i;
		has_variable = true;
	}
	
	public void setVariable(int i, String name) {
		variable = i;
		var_name = name;
		has_variable = true;
	}
	
	public void setConstant(int c) {
		has_constant = true;
		constant_val = "" + c;
	}
	
	public void setConstant(String c) {
		has_constant = true;
		constant_val = c;
	}
	
	public void setType(var_type v) {
		variable_type = v;
	}
	
	public void addToPhi(int var) {
		oper = ari_math_operator.PHI;
		has_phi = true;
		phis.add(var);
	}
	
	public int getVariable() {
		return variable;
	}
	
	public String getVariableName() {
		return var_name;
	}
	
	public boolean hasVariable() {
		return has_variable;
	}
	
	public boolean getLocked() {
		return locked;
	}
	
	public MathNumber getLeft() {
		return left;
	}
	
	public MathNumber getRight() {
		return right;
	}
	
	public ari_math_operator getOper() {
		return oper;
	}
	
	public Set<Integer> getPhiSet() {
		return phis;
	}
	
	public void lock(HashSet<Integer> param_set, String method_name) {
		if (!locked) {
			if (param_set.contains(variable)) {
				this.method_name = "PARAMETER";
			} else {
				this.method_name = method_name;
			}
			locked = true;
		}
	}
	
	public void reconfigure(HashMap<Integer, Integer> map_param_to_current, IR ir, int instruction_line) {
		if (map_param_to_current.containsKey(variable)) {
			if(locked) {
				variable = map_param_to_current.get(variable);
				locked = false;
				method_name = "";
				has_constant = false;
				String[] t1 = ir.getLocalNames(instruction_line, variable);
				var_name = "";
				if(t1.length >= 1) {
					var_name = ir.getLocalNames(instruction_line, variable)[0];
				}
				if (ir.getSymbolTable().isConstant(variable)) {
					has_constant = true;
					if(ir.getSymbolTable().isNumberConstant(variable)) {
						constant_val = "" + ir.getSymbolTable().getIntValue(variable);
					}
					if(ir.getSymbolTable().isStringConstant(variable)) {
						constant_val = ir.getSymbolTable().getStringValue(variable);
					}
				}
			}
		}
	}
	
	public String toString() {
		if (oper.equals(ari_math_operator.NONE)) {
			if(has_variable) {
				String to_return = "";
				to_return += "v" + variable;
				if (locked) {
					to_return += "[" + method_name + "]";
				}
				if (!var_name.equals("")) {
					to_return += "[=" + var_name + "]";
				} 
				if (has_constant) {
					to_return += "#:" + constant_val;
				}
				return to_return;
			} else {
				return "Invalid";
			}
		}
		if (oper.equals(ari_math_operator.PHI)) {
			String temp = "Phi(";
			boolean first = true;
			for(int p : phis) {
				if(first) {
					temp = temp + "v" + p;
					first = false;
				} else {
					temp = temp + ", v" + p;
				}
			}
			temp = temp + ")";
			return temp;
		}
		if (oper.equals(ari_math_operator.ADD)) {
			return "(" +left + " + " + right +")";
		}
		if (oper.equals(ari_math_operator.MULT)) {
			return "(" +left + " * " + right +")";
		}
		if (oper.equals(ari_math_operator.DIV)) {
			return "(" +left + " / " + right +")";
		}
		if (oper.equals(ari_math_operator.SUB)) {
			return "(" +left + " - " + right +")";
		}
		
		return "Invalid";
	}
}
