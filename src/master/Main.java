package master;

import java.io.IOException;
import java.util.HashSet;

import com.ibm.wala.cast.python.client.PythonAnalysisEngine;
import com.ibm.wala.cast.python.test.TestPythonCallGraphShape;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.callgraph.CallGraphBuilder;
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey;
import com.ibm.wala.ipa.cha.ClassHierarchyException;
import com.ibm.wala.util.CancelException;
import com.ibm.wala.util.NullProgressMonitor;

public class Main {

	
	
	//Changes made to WALA. New empty class added and process was made public.
	// com.ibm.wala.core
	// com.ibm.wala.cfg util
	// getTakenSuccessor
	public static void main(String[] args) {
		HashSet<String> ignored_methods = new HashSet<String>();
		ignored_methods.add("FakeRootClass");
		TestPythonCallGraphShape driver = new TestPythonCallGraphShape() {
			
		};
		
		PreconditionFinder pf = new PreconditionFinder();
		// TODO Auto-generated method stub
		System.err.println("HELLO");
		
		CallGraph CG = null;
		try {
			PythonAnalysisEngine<?> E = driver.makeEngine("/home/andy/eclipse-workspace/master_project/bin/main_input.py");
			CallGraphBuilder<? super InstanceKey> builder = E.defaultCallGraphBuilder();
			CG = builder.makeCallGraph(E.getOptions(), new NullProgressMonitor());
			//CG = driver.process("/home/andy/eclipse-workspace/master_project/bin/main_input.py");
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
