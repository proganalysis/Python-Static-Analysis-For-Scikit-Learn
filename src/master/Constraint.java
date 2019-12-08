package master;

import java.util.HashMap;
import java.util.HashSet;

import com.ibm.wala.ssa.IR;

public class Constraint {
	
	// If NOT OR NONE than only right should be set.
	enum bool_operator {
		AND, OR, NONE, NOT;
	}
	private boolean always_true;
	private Constraint left;
	private Constraint right;
	private bool_operator linker;
	
	Constraint() {
		left = null;
		right = null;
		linker = bool_operator.NONE;
		always_true = false;
	}
	
	Constraint(Constraint c) {
		left = null;
		right = null;
		if(c.left != null) {
			if (c.left instanceof MathConstraint) {
				left = new MathConstraint((MathConstraint) c.left);
			} else {
				left = new Constraint(c.left);
			}
		}
		if(c.right != null) {
			if (c.right instanceof MathConstraint) {
				right = new MathConstraint((MathConstraint) c.right);
			} else {
				right = new Constraint(c.right);
			}
		}
		linker = c.linker;
		always_true = c.always_true;
	}
	
	Constraint(Constraint l, Constraint r, bool_operator link) {
		left = l;
		right = r;
		linker = link;
		always_true = false;
	}
	
	public static Constraint getCopy(Constraint c) {
		if (c instanceof MathConstraint) {
			return getCopy((MathConstraint) c);
		}
		return new Constraint(c);
	}
	
	public static Constraint getCopy(MathConstraint c) {
		return new MathConstraint(c);
	}
	
	public void FindAndReplace(int variable_to_find, MathNumber replacement) {
		if(this instanceof MathConstraint) {
			((MathConstraint)this).FindAndReplace2(variable_to_find, replacement);
		}
		if(left != null) {
			left.FindAndReplace(variable_to_find, replacement);
		}
		if(right != null) {
			right.FindAndReplace(variable_to_find, replacement);
		}
	}
	
	// lock is used once we're done finding a precondition.
	public void lock(HashSet<Integer> param_set, String method_name) {
		if(this instanceof MathConstraint) {
			((MathConstraint)this).lock2(param_set, method_name);
		}
		if(left != null) {
			left.lock(param_set, method_name);
		}
		if(right != null) {
			right.lock(param_set, method_name);
		}
	}
	
	// reconfigure is used, once we need to use a precondition from another function.
	public void reconfigure(HashMap<Integer, Integer> map_param_to_current, IR ir, int instruction_line) {
		if(this instanceof MathConstraint) {
			((MathConstraint)this).reconfigure2(map_param_to_current, ir, instruction_line);
		}
		if(left != null) {
			left.reconfigure(map_param_to_current, ir, instruction_line);
		}
		if(right != null) {
			right.reconfigure(map_param_to_current, ir, instruction_line);
		}
	}
	
	void setLinker(bool_operator l) {
		linker = l;
	}
	
	void setLeft(Constraint c) {
		left = c;
	}
	
	void setRight(Constraint c) {
		right = c;
	}
	
	void setAlwaysTrue(boolean t) {
		always_true = t;
	}
	
	public String toString() {
		if (always_true) {
			return "TRUE";
		}
		if(right == null) {
			return "";
		}
		if (linker.equals(bool_operator.NONE)) {
			return "(" + right.toString() + ")";
		}
		if (linker.equals(bool_operator.NOT)) {
			return "(!" + right.toString() + ")";
		}
		String temp = "";
		if (linker.equals(bool_operator.AND)) {
			temp = "&&";
		}
		if (linker.equals(bool_operator.OR)) {
			temp = "||";
		}
		return "(" + left.toString() + ") " + temp + " (" + right.toString() + ")";
	}
}
