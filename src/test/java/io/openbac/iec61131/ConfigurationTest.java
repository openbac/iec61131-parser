package io.openbac.iec61131;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

public class ConfigurationTest extends GrammarTest {

	@Test
	public void test() throws IOException {
		String cfg="CONFIGURATION TestConfiguration\n" + 
				"    VAR_GLOBAL\n" + 
				"        b_Start_Stop  : BOOL;         \n" + 
				"        b_ON_OFF      : BOOL;         \n" + 
				"        Start_Stop AT %IX0.0:BOOL;    \n" + 
				"        ON_OFF     AT %QX0.0:BOOL;    \n" + 
				"    END_VAR\n" + 
				"    \n" + 
				"    VAR_GLOBAL RETAIN\n" + 
				"    		b_retain : BOOL;\n" + 
				"    	END_VAR\n" + 
				"    	\n" + 
				"    	VAR_GLOBAL CONSTANT\n" + 
				"    		r_pi 	: REAL;\n" + 
				"    	END_VAR\n" + 
				"    	\n" + 
				"\n" + 
				"/* \n" + 
				"multiline comment\n" + 
				"*/\n" + 
				"\n" + 
				"// single line comment\n" + 
				"\n" + 
				"(* classic comment *)\n" + 
				"\n" + 
				"\n" + 
				"    TASK Tick(INTERVAL := T#20ms, PRIORITY:=1);\n" + 
				"\n" + 
				"    PROGRAM Main WITH Tick : Monitor_Start_Stop;\n" + 
				"END_CONFIGURATION";
		assertGrammarRulePositive(cfg, "library_element_declaration");

	}

}
