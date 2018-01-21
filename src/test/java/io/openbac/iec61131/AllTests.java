package io.openbac.iec61131;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({ DatatypeTest.class, FunctionBlockTest.class, FunctionTest.class, StatementTest.class, ConfigurationTest.class, ProgramTest.class })
public class AllTests {

}
