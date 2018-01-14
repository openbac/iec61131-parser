package io.openbac.iec61131;

import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.Charset;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;

public abstract class GrammarTest {

	protected void assertGrammarRulePositive(String arg, String ruleName) throws IOException {

		ByteArrayInputStream bain;
		IEC61131_3Parser parser;
		IEC61131_3Lexer lexer;

		bain = new ByteArrayInputStream(arg.getBytes(Charset.forName("UTF-8")));

		ANTLRInputStream as = new ANTLRInputStream(bain);

		lexer = new IEC61131_3Lexer((CharStream) as);

		CommonTokenStream ts = new CommonTokenStream(lexer);

		parser = new IEC61131_3Parser(ts);

		parser.setErrorHandler(new BailErrorStrategy());

		Method ruleMethod = null;

		try {
			ruleMethod = IEC61131_3Parser.class.getDeclaredMethod(ruleName);
		} catch (NoSuchMethodException e) {
			fail("Parser Rule not found !? \n" + e.getMessage());
		} catch (SecurityException e) {
			fail(e.getMessage());
		}

		try {

			ParseTree tree = (ParseTree) ruleMethod.invoke(parser);

			System.out.println(tree.toStringTree(parser));

		} catch (InvocationTargetException e) {

			ParseCancellationException ex = (ParseCancellationException) e.getCause();

			RecognitionException rex = (RecognitionException) ex.getCause();
			rex.printStackTrace();

			fail(rex.getOffendingToken().toString());

		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			fail();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			fail();
		}

	}

}
