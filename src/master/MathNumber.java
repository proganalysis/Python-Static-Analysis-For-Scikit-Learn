package master;

import java.util.HashSet;
import java.util.Set;

public class MathNumber {
	
	// If None, nothing is set. Phi is special
	enum ari_math_operator {
		ADD, SUB, MULT, DIV, PHI, NONE;
	}
	
	private int constant_val;
	private int variable;
	private String var_name;
	private boolean has_constant;
	private boolean has_variable;
	//if has_phi is true, left and right have to be vars
	private boolean has_phi;
	private Set<Integer> phis;
	private MathNumber left;
	private MathNumber right;
	private ari_math_operator oper; 
	MathNumber() {
		constant_val = 0;
		variable = 0;
		has_constant = false;
		has_variable = false;
		has_phi = false;
		left = null;
		right = null;
		var_name = "";
		oper = ari_math_operator.NONE;
		phis = new HashSet<Integer>();
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
	}
	
	MathNumber(int var1, int var2, String name1, String name2, ari_math_operator op) {
		constant_val = 0;
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
		constant_val = c;
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
	
	public String toString() {
		if (oper.equals(ari_math_operator.NONE)) {
			if(has_variable) {
				if (var_name.equals("")) {
					if(has_constant) {
						return "v" + variable + "#:" + constant_val;
					} else {
						return "v" + variable;
					}
				} else {
					if(has_constant) {
						return "v" + variable + "[=" + var_name + "]#:" + constant_val;
					} else {
						return "v" + variable + "[=" + var_name + "]";
					}
				}
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
		
		return "Invalid";
	}
}
