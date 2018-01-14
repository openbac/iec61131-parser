package io.openbac.iec61131;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

public class StatementTest extends GrammarTest {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void test() throws IOException {
		assertGrammarRulePositive("var1 := (1 + 0);", "statement");
		assertGrammarRulePositive("var1 := (1 + 1);", "statement");
		assertGrammarRulePositive("var1 := ((1 + 0)*10)+5*3-2*4;", "statement");
		assertGrammarRulePositive("a := (1<>0)", "statement");

	}

}
