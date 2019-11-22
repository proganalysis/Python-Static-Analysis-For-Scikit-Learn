package master;

import com.ibm.wala.cast.python.ssa.PythonInstructionVisitor;
import com.ibm.wala.ssa.SSABinaryOpInstruction;
import com.ibm.wala.ssa.SSAConditionalBranchInstruction;
import com.ibm.wala.ssa.SSAGotoInstruction;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAPhiInstruction;
import com.ibm.wala.ssa.SSAThrowInstruction;

public class CustomVisitor extends SSAInstruction.Visitor {
	
	private boolean is_if;
	private boolean is_throw;
	private boolean is_binary_op;
	private boolean is_phi;
	private boolean is_goto;
	CustomVisitor() {
		is_if = false;
		is_throw = false;
	}
	
	public boolean getIf() {
		boolean temp = is_if;
		is_if = false;
		return temp;
	}
	
	public boolean getThrow() {
		boolean temp = is_throw;
		is_throw = false;
		return temp;
	}
	
	public boolean getBinaryOp() {
		boolean temp = is_binary_op;
		is_binary_op = false;
		return temp;
	}
	
	public boolean GetIsPhi() {
		boolean temp = is_phi;
		is_phi = false;
		return temp;
	}
	
	public boolean GetIsGoto() {
		boolean temp = is_goto;
		is_goto = false;
		return temp;
	}
	
	@Override
	public void visitConditionalBranch(SSAConditionalBranchInstruction instruction) {
		is_if = true;
	}
	
	@Override
	public void visitThrow(SSAThrowInstruction instruction) {
		is_throw = true;
	}
	
	@Override
	public void visitBinaryOp(SSABinaryOpInstruction instruction) {
		is_binary_op = true;
	}
	
	@Override
	public void visitPhi(SSAPhiInstruction instruction) {
		is_phi = true;
	}
	
	@Override
	public void visitGoto(SSAGotoInstruction instruction) {
		is_goto = true;
	}
}
