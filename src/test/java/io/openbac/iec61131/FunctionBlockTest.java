package io.openbac.iec61131;

import java.io.IOException;

import org.junit.Test;

public class FunctionBlockTest extends GrammarTest {

	@Test
	public void test() throws IOException {
	
		String fb = 	"FUNCTION_BLOCK test\n" + 
					"VAR_INPUT			\n" + 
					" a: BOOL := 1; 		\n" + 
					"END_VAR				\n" + 
					"a:=1;				\n" +
					"a:= ABS(-1);		\n" + 
					"END_FUNCTION_BLOCK	\n";

		assertGrammarRulePositive(fb, "library_element_declaration");
		
	}

}
