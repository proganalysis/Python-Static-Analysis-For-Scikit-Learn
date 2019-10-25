package master;

public class MathNumber {
	
	// If None, nothing is set. Phi is special
	enum ari_math_operator {
		ADD, SUB, MULT, DIV, PHI, NONE;
	}
	
	private int constant;
	private int variable;
	private String var_name;
	private boolean has_constant;
	private boolean has_variable;
	//if has_phi is true, left and right have to be vars
	private boolean has_phi;
	private MathNumber left;
	private MathNumber right;
	private ari_math_operator oper; 
	MathNumber() {
		constant = 0;
		variable = 0;
		has_constant = false;
		has_variable = false;
		has_phi = false;
		left = null;
		right = null;
		var_name = "";
		oper = ari_math_operator.NONE;
	}
	
	MathNumber(MathNumber mn) {
		constant = mn.constant;
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
	}
	
	MathNumber(int var1, int var2, String name1, String name2, ari_math_operator op) {
		constant = 0;
		variable = 0;
		has_constant = false;
		has_variable = false;
		left = new MathNumber();
		right = new MathNumber();
		left.setVariable(var1, name1);
		right.setVariable(var2, name2);
		oper = op;
		var_name = "";
	}
	
	public void assign(MathNumber nm) {
		constant = nm.constant;
		variable = nm.variable;
		has_constant = nm.has_constant;
		has_variable = nm.has_variable;
		left = nm.left;
		right = nm.right;
		oper = nm.oper;
		var_name = nm.var_name;
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
	
	public String toString() {
		if (oper.equals(ari_math_operator.NONE)) {
			if(has_variable) {
				if (var_name.equals("")) {
					return "v" + variable;
				} else {
					return "v" + variable + "[=" + var_name + "]";
				}
			} else {
				return "Invalid";
			}
		}
		if (oper.equals(ari_math_operator.PHI)) {
			return "Phi(" + left + ", " + right + ")";
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
