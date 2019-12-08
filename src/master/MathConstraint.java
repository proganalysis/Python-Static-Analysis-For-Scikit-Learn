package master;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;

import com.ibm.wala.ssa.IR;

import master.MathNumber.ari_math_operator;

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
			MathNumber t = queue.poll();
			if(t.hasVariable() && t.getVariable() == variable_to_find) {
				t.assign(replacement);
			}
			if(t.getOper().equals(ari_math_operator.PHI)) {
				if(t.getPhiSet().contains(variable_to_find)) {
					t.assign(replacement);
				}
				continue;
			}
			if (t.getLeft() != null) {
				queue.add(t.getLeft());
			}
			
			if (t.getRight() != null) {
				queue.add(t.getRight());
			}
			
		}
	}
	
	protected void lock2 (HashSet<Integer> param_set, String method_name) {
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
			t.lock(param_set, method_name);
		}
	}
	
	protected void reconfigure2(HashMap<Integer, Integer> map_param_to_current, IR ir, int instruction_line) {
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
			t.reconfigure(map_param_to_current, ir, instruction_line);
		}
	}
	
	public String toString() {
		if (linker.equals(bool_math_operator.EQ)) {
			return left2.toString() + " = " + right2.toString();
		}
		if (linker.equals(bool_math_operator.NEQ)) {
			return left2.toString() + " != " + right2.toString();
		}
		if (linker.equals(bool_math_operator.G)) {
			return left2.toString() + " > " + right2.toString();
		}
		if (linker.equals(bool_math_operator.L)) {
			return left2.toString() + " < " + right2.toString();
		}
		if (linker.equals(bool_math_operator.GEQ)) {
			return left2.toString() + " >= " + right2.toString();
		}
		if (linker.equals(bool_math_operator.LEQ)) {
			return left2.toString() + " <= " + right2.toString();
		}
		return "";
	}
}
