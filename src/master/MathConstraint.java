package master;

import java.util.LinkedList;
import java.util.Queue;

import com.ibm.wala.ssa.ISSABasicBlock;

public class MathConstraint extends Constraint {

	enum bool_math_operator {
		EQ, NEQ, GEQ, G, LEQ, L;
	}
	
	private MathNumber left2;
	private MathNumber right2;
	private bool_math_operator linker;
	
	MathConstraint(MathNumber l, MathNumber r, bool_math_operator link) {
		left2 = l;
		right2 = r;
		linker = link;
	}
	
	MathConstraint(MathConstraint mc) {
		super((Constraint)mc);
		left2 = null;
		right2 = null;
		if (mc.left2 != null) {
			left2 = new MathNumber(mc.left2);
		}
		if (mc.right2 != null) {
			right2 = new MathNumber(mc.right2);
		}
		linker = mc.linker;
	}
	
	protected void FindAndReplace2(int variable_to_find, MathNumber replacement) {
		Queue<MathNumber> queue = new LinkedList<>();
		if (left2 != null) {
			queue.add(left2);
		}
		if (right2 != null) {
			queue.add(right2);
		}
		while (!queue.isEmpty()) {
			System.err.println("Looking for " + variable_to_find);
			MathNumber t = queue.poll();
			if (t.getLeft() != null) {
				queue.add(t.getLeft());
			}
			
			if (t.getRight() != null) {
				queue.add(t.getRight());
			}
			if(t.hasVariable() && t.getVariable() == variable_to_find) {
				System.err.println("Found");
				t.assign(replacement);
			}
		}
	}
	
	protected void FindAndReplaceVar2(int variable_to_find, int variable_to_replace) {
		Queue<MathNumber> queue = new LinkedList<>();
		if (left2 != null) {
			queue.add(left2);
		}
		if (right2 != null) {
			queue.add(right2);
		}
		while (!queue.isEmpty()) {
			MathNumber t = queue.poll();
			if (t.getLeft() != null) {
				queue.add(t.getLeft());
			}
			
			if (t.getRight() != null) {
				queue.add(t.getRight());
			}
			if(t.hasVariable() && t.getVariable() == variable_to_find) {
				t.setVariable(variable_to_replace);
			}
		}
	}
	
	protected void FindAndReplace2(String variable_to_find, MathNumber replacement) {
		Queue<MathNumber> queue = new LinkedList<>();
		if (left2 != null) {
			queue.add(left2);
		}
		if (right2 != null) {
			queue.add(right2);
		}
		while (!queue.isEmpty()) {
			System.err.println("Looking for " + variable_to_find);
			MathNumber t = queue.poll();
			if (t.getLeft() != null) {
				queue.add(t.getLeft());
			}
			
			if (t.getRight() != null) {
				queue.add(t.getRight());
			}
			if(t.hasVariable() && t.getVariableName().equals(variable_to_find)) {
				System.err.println("Found");
				//t.assign(replacement);
			}
		}
	}
	
	public String toString() {
		if (linker.equals(bool_math_operator.EQ)) {
			return left2.toString() + " = " + right2.toString();
		}
		if (linker.equals(bool_math_operator.NEQ)) {
			return left2.toString() + " != " + right2.toString();
		}
		return "";
	}
}
