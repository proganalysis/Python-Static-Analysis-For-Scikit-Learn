package master;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;

import com.ibm.wala.cast.ipa.callgraph.CAstCallGraphUtil;
import com.ibm.wala.cast.python.client.PythonAnalysisEngine;
import com.ibm.wala.cast.python.test.TestPythonCallGraphGenerator;
import com.ibm.wala.cast.python.test.TestPythonCallGraphShape;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.propagation.PropagationCallGraphBuilder;
import com.ibm.wala.ipa.callgraph.propagation.SSAContextInterpreter;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.ssa.ISSABasicBlock;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAInstruction.IVisitor;
import com.ibm.wala.ssa.SSAInstruction.Visitor;
import com.ibm.wala.util.CancelException;

public class Main {

	
	
	//Changes made to WALA. New empty class added and process was made public.
	// com.ibm.wala.core
	// com.ibm.wala.cfg util
	// getTakenSuccessor
	public static void main(String[] args) {
		HashSet<String> ignored_methods = new HashSet<String>();
		ignored_methods.add("FakeRootClass");
		ignored_methods.add("MAGIC_EQ");
		TestPythonCallGraphGenerator cg = new TestPythonCallGraphGenerator();
		PreconditionFinder pf = new PreconditionFinder();
		// TODO Auto-generated method stub
		System.err.println("HELLO");
		
		CallGraph CG = null;
		try {
			CG = cg.process("main_input.py");
		} catch (ClassHierarchyException | IllegalArgumentException | CancelException | IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.err.println(CG);
		
		System.err.println("seprate");
		CG.forEach((n) -> {
			
			String method_name = n.getIR().getMethod().getDeclaringClass().getName().getClassName().toString();
			if (ignored_methods.contains(method_name)) {
				return;
			}
			pf.findWeakest(n.getIR());
			// System.err.println("Big Duck");
			// System.err.println(n.getIR().getMethod());
			 System.err.println(n.getIR());
		});
		
		//System.err.println("Done2");
		//verifyGraphAssertions(CG, assertionsCalls2);
		System.err.println("Done");
		
	}

}
