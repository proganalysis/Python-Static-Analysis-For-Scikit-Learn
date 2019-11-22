package master;

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
	
	public void FindAndReplaceVar(int variable_to_find, int variable_to_replace) {
		if(this instanceof MathConstraint) {
			((MathConstraint)this).FindAndReplaceVar2(variable_to_find, variable_to_replace);
		}
		if(left != null) {
			left.FindAndReplaceVar(variable_to_find, variable_to_replace);
		}
		if(right != null) {
			right.FindAndReplaceVar(variable_to_find, variable_to_replace);
		}
	}
	
	public void FindAndReplace(String variable_to_find, MathNumber replacement) {
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
