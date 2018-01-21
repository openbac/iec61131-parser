package io.openbac.iec61131;

import java.io.IOException;

import org.junit.Test;

public class ProgramTest extends GrammarTest {

	
	@Test
	public void test() throws IOException {
		
		String prg = "PROGRAM Monitor_Start_Stop          // Actual Program\n" + 
				"    VAR_EXTERNAL\n" + 
				"        Start_Stop  : BOOL;\n" + 
				"        ON_OFF      : BOOL;\n" + 
				"    END_VAR\n" + 
				"    VAR                             // Temporary variables for logic handling\n" + 
				"        ONS_Trig    : BOOL;\n" + 
				"        Rising_ONS  : BOOL;\n" + 
				"    END_VAR\n" + 
				"\n" + 
				"    // Start of Logic\n" + 
				"    // Catch the Rising Edge One Shot of the Start_Stop input\n" + 
				"    ONS_Trig    := Start_Stop AND NOT Rising_ONS;\n" + 
				"    \n" + 
				"    // Main Logic for Run_Contact -- Toggle ON / Toggle OFF ---\n" + 
				"    ON_OFF := (ONS_Trig AND NOT ON_OFF) OR (ON_OFF AND NOT ONS_Trig);\n" + 
				"\n" + 
				"    // Rising One Shot logic   \n" + 
				"    Rising_ONS := Start_Stop;\n" + 
				"END_PROGRAM";
		assertGrammarRulePositive(prg, "library_element_declaration");
	}
	
	
}
