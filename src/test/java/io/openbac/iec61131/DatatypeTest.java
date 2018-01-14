package io.openbac.iec61131;

import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class DatatypeTest extends GrammarTest {

	@Test
	public void testIntegerDatatypes() throws IOException {

		assertGrammarRulePositive("123", "integer_literal");
		assertGrammarRulePositive("2#01012030101", "integer_literal");
		assertGrammarRulePositive("INT#123", "integer_literal");
		assertGrammarRulePositive("INT#2#01012030101", "integer_literal");
		assertGrammarRulePositive("UINT#123", "integer_literal");
		assertGrammarRulePositive("UINT#2#01012030101", "integer_literal");
		assertGrammarRulePositive("16#affe123", "integer_literal");

	}
	
	@Test
	public void testRealDatatypes() throws IOException {

		assertGrammarRulePositive("REAL#1.1", "real_literal");
		assertGrammarRulePositive("LREAL#1.1", "real_literal");
		
	}

	
	@Test
	public void testBoolDatatype() throws IOException {

		assertGrammarRulePositive("BOOL#0", "boolean_literal");
		assertGrammarRulePositive("BOOL#1", "boolean_literal");
		assertGrammarRulePositive("BOOL#FALSE", "boolean_literal");
		assertGrammarRulePositive("BOOL#TRUE", "boolean_literal");

	}
	
	@Test
	public void testBitstringDatatype() throws IOException {

		assertGrammarRulePositive("BYTE#0", "bit_string_literal");
		assertGrammarRulePositive("WORD#1", "bit_string_literal");
		assertGrammarRulePositive("DWORD#1", "bit_string_literal");
		assertGrammarRulePositive("LWORD#0", "bit_string_literal");

	}
	
	@Test
	public void testStringDatatype() throws IOException {

		assertGrammarRulePositive("'TestString'", "character_string");
		assertGrammarRulePositive("\"TestString\"", "character_string");


	}
}
