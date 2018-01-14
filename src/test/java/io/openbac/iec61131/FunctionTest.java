package io.openbac.iec61131;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

public class FunctionTest extends GrammarTest {

	@Test
	public void testSimpleFunction() throws IOException  {
	
		String test = 	"FUNCTION test : INT\n" + 
					"VAR_INPUT			\n" + 
					" a: INT := 1; 		\n" + 
					"END_VAR				\n" + 
					"a:=1;				\n" +
					"a:= ABS(-1);		\n"+
					"RETURN a;	\n" + 
					"END_FUNCTION\n";

		assertGrammarRulePositive(test, "library_element_declaration");
		
	}
	
	@Test
	public void testStandardFuncA() throws IOException {
		
		String test ="FUNCTION SIMPLE_FUN : REAL "+
				"VAR_INPUT\n" + 
				"A : REAL ; 		(* Festlegung der externen Schnittstelle *) " +
				"C   : REAL := 1.0;					\n" + 
				"END_VAR								\n" + 
				"VAR_IN_OUT" +
				"COUNT : INT ; "+
				"END_VAR\n" + 
				"VAR "+
				"COUNTP1 : INT ; "+
				"END_VAR\n" + 
				"COUNTP1 := ADD(COUNT,1); (*Festlegung des Funktionsrumpfs *) "+
				"COUNT := COUNTP1 ;\n" + 
				"SIMPLE_FUN := A*B/C;\n" + 
				"END_FUNCTION";
		
		assertGrammarRulePositive(test, "library_element_declaration");
	}

	
	
}
