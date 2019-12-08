package master;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;

import com.ibm.wala.cast.loader.AstMethod;
import com.ibm.wala.cast.python.client.PythonAnalysisEngine;
import com.ibm.wala.cast.python.test.TestPythonCallGraphShape;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.CallGraphBuilder;
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.util.CancelException;
import com.ibm.wala.util.NullProgressMonitor;

public class Main {

	public static void main(String[] args) {
		HashSet<String> ignored_methods = new HashSet<String>();
		ignored_methods.add("FakeRootClass");
		TestPythonCallGraphShape driver = new TestPythonCallGraphShape() {};
		
		PreconditionFinder pf = new PreconditionFinder();
		
		CallGraph CG = null;
		try {
			// The absolute path to your python file.
			PythonAnalysisEngine<?> E = driver.makeEngine("/home/andy/eclipse-workspace/master_project/bin/main_input.py");
			CallGraphBuilder<? super InstanceKey> builder = E.defaultCallGraphBuilder();
			CG = builder.makeCallGraph(E.getOptions(), new NullProgressMonitor());
		} catch (ClassHierarchyException | IllegalArgumentException | CancelException | IOException e) {
			e.printStackTrace();
		}
		System.err.println(CG);
		
		System.err.println("Start Precondition finding");
		boolean done_checking = pf.GetRedo();
		
		// Redo the loop if we found a precondition from a function.
		// It's possible we can use that to find precondition from other functions.
		while(done_checking) {
			pf.setRedo(false);
			CG.forEach((n) -> {
				ArrayList<Integer> param_arr = new ArrayList<Integer>();
				HashSet<Integer> param_set = new HashSet<Integer>();
				String method_name = n.getIR().getMethod().getDeclaringClass().getName().getClassName().toString();
				
				if (ignored_methods.contains(method_name)) {
					return;
				}
				
				// Add parameters to the param_arr and param_set.
				if (n.getIR().getMethod() instanceof AstMethod) {
					AstMethod temp = (AstMethod)n.getIR().getMethod();
					for (int i = 1; i < temp.getNumberOfParameters(); i++) {
						param_arr.add(temp.symbolTable().getParameter(i));
						param_set.add(temp.symbolTable().getParameter(i));
					}		
				}
				
				boolean found_throw = pf.findWeakest(n.getIR(), method_name, param_set, param_arr);
				// Once we found a precondition, we don't need to find it again.
				if (found_throw) {
					ignored_methods.add(method_name);
				}
				System.err.println("Debug IR for function " + method_name);
				System.err.println(n.getIR());
			});
			done_checking = pf.GetRedo();
		}
		System.err.println("Done");
		
	}

}
