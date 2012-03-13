/*
 * Copyright (C) 2012 Jimmy Liang <http://gsd.uwaterloo.ca>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.clafer.ig;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.Pair;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.CommandScope;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.parser.AlloyCompiler;
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;
import java.io.EOFException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import static org.clafer.ig.Util.*;

public final class AlloyIG {

    // The only to detect unsatisfiability to is wait for the minimized callback to be called.
    // If it is called, then unsatisfiable. Otherwise not. Very strange way of doing things
    // but that's how its done in Alloy as far as I can tell.
    // See alloy4whole/SimpleReporter.java for details.
    private static class AlloyIGReporter extends A4Reporter {

        private int minimizedBefore;
        private int minimizedAfter;

        @Override
        public void warning(ErrorWarning msg) {
            System.err.print("Relevance Warning:\n" + (msg.toString().trim()) + "\n\n");
            System.err.flush();
        }

        @Override
        public void minimized(Object command, int before, int after) {
            minimizedBefore = before;
            minimizedAfter = after;
            super.minimized(command, before, after);
        }
    };

    private static interface Operation {
    }

    private static class ResolveOperation implements Operation {
    }

    private static class NextOperation implements Operation {
    }

    private static class CounterexampleOperation implements Operation {
    }

    private static class QuitOperation implements Operation {
    }

    private static class SetGlobalScopeOperation implements Operation {

        private final int scopeSize;

        public SetGlobalScopeOperation(int scopeSize) {
            this.scopeSize = scopeSize;
        }

        public int getScopeSize() {
            return scopeSize;
        }
    }

    private static class SetScopeOperation implements Operation {

        private final String sig;
        private final int scopeSize;

        public SetScopeOperation(String sig, int scopeSize) {
            if (sig == null) {
                throw new NullPointerException();
            }
            this.sig = sig;
            this.scopeSize = scopeSize;
        }

        public String getSig() {
            return sig;
        }

        public int getScopeSize() {
            return scopeSize;
        }
    }

    private static Operation nextOperation() throws IOException {
        String op = readMessage();
        if (op == null || op.equals("quit")) {
            return new QuitOperation();
        } else if (op.equals("next")) {
            return new NextOperation();
        } else if (op.equals("setGlobalScope")) {
            int scopeSize = Integer.parseInt(readMessage());
            return new SetGlobalScopeOperation(scopeSize);
        } else if (op.equals("setScope")) {
            String sig = readMessage();
            int scopeSize = Integer.parseInt(readMessage());
            return new SetScopeOperation(sig, scopeSize);
        } else if (op.equals("resolve")) {
            return new ResolveOperation();
        } else if (op.equals("counterexample")) {
            return new CounterexampleOperation();
        }
        throw new AlloyIGException("Unknown op " + op);
    }

    private static CommandScope setCommandScopeSize(int scopeSize, CommandScope cs) throws ErrorSyntax {
        return new CommandScope(
                cs.pos, cs.sig, cs.isExact, scopeSize, scopeSize, cs.increment);
    }

    private static List<CommandScope> setScopeSize(Sig sig, int scopeSize, List<CommandScope> scope) throws ErrorSyntax {
        List<CommandScope> newScope = new ArrayList<CommandScope>();

        boolean found = false;
        for (CommandScope cs : scope) {
            if (sig.equals(cs.sig)) {
                found = true;
                newScope.add(setCommandScopeSize(scopeSize, cs));
            } else {
                newScope.add(cs);
            }
        }

        if (!found) {
            newScope.add(new CommandScope(sig, false, scopeSize));
        }

        return newScope;
    }

    private static String toXml(A4Solution ans) throws Err, IOException {
        StringWriter xml = new StringWriter();
        ans.writeXML(new PrintWriter(xml), null, null);
        return xml.toString();
    }

    private static String multiplicity(Sig sig) {
        if (sig.isOne != null) {
            return "One";
        } else if (sig.isLone != null) {
            return "Lone";
        } else if (sig.isSome != null) {
            return "Some";
        }
        return "Any";
    }

    public static void main(String[] args) throws IOException, Err {
        try {
            System.loadLibrary("minisatprover");
        } catch (UnsatisfiedLinkError e) {
            System.loadLibrary("minisatproverx1");
        }
        try {
            run(args);
        } catch (EOFException e) {
            System.err.println("AlloyIG stream closed.");
        }
    }

    private static String removeCurly(String string) {
        int length = string.length();
        if (length > 0 && string.charAt(0) == '{' && string.charAt(length - 1) == '}') {
            return string.substring(1, length - 1);
        }
        throw new IllegalArgumentException("\"" + string + "\" is not surrounded by curly brackets.");
    }

    public static void run(String[] args) throws IOException, Err {
        AlloyIGReporter rep = new AlloyIGReporter();
        String modelVerbatim = readMessage();

        // Parse+typecheck the model
        CompModule world = AlloyCompiler.parse(rep, modelVerbatim);
        SafeList<Sig> sigs = world.getAllSigs();

        Command command = world.getAllCommands().get(0);

        // Send back all the sigs
        writeMessage(Integer.toString(sigs.size()));
        for (Sig sig : sigs) {
            writeMessage(sig.label);
            writeMessage(multiplicity(sig));
            writeMessage(sig instanceof Sig.PrimSig ? "" : removeCurly(sig.type().toString()));
        }
        // Send back the global scope
        writeMessage(Integer.toString(command.overall));

        A4Solution ans = null;
        Operation operation = null;

        // Choose some default options for how you want to execute the commands
        A4Options options = new A4Options();
        options.coreMinimization = 0;
        options.solver = A4Options.SatSolver.MiniSatProverJNI;

        while (!(operation instanceof QuitOperation)) {
            operation = nextOperation();

            if (operation instanceof ResolveOperation) {
                rep.minimizedBefore = 0;
                rep.minimizedAfter = 0;
                // Reexecute the command
                ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
            } else if (operation instanceof NextOperation) {
                if (ans.satisfiable()) {
                    writeMessage("True");
                    StringWriter xml = new StringWriter();
                    ans.writeXML(new PrintWriter(xml), null, null);
                    writeMessage(xml.toString());

                    A4Solution nextAns = ans.next();
                    if (nextAns == ans) {
                        break;
                    }
                    ans = nextAns;
                } else {
                    writeMessage("False");
                }
            } else if (operation instanceof SetGlobalScopeOperation) {
                SetGlobalScopeOperation increaseScope = (SetGlobalScopeOperation) operation;
                int scopeSize = increaseScope.getScopeSize();

                Command c = command;
                command = new Command(c.pos, c.label, c.check, scopeSize, c.bitwidth, c.maxseq, c.expects, c.scope, c.additionalExactScopes, c.formula, c.parent);
            } else if (operation instanceof SetScopeOperation) {
                SetScopeOperation increaseScope = (SetScopeOperation) operation;
                String sigName = increaseScope.getSig();
                int scopeSize = increaseScope.getScopeSize();

                Sig sig = findSig(sigName, sigs);
                List<CommandScope> scope = setScopeSize(sig, scopeSize, command.scope);

                Command c = command;
                command = new Command(c.pos, c.label, c.check, c.overall, c.bitwidth, c.maxseq, c.expects, scope, c.additionalExactScopes, c.formula, c.parent);
            } else if (operation instanceof CounterexampleOperation) {
                AlloyIGReporter reporter = new AlloyIGReporter();
                reporter.minimizedBefore = rep.minimizedBefore;
                reporter.minimizedAfter = rep.minimizedAfter;


                Set<Pos> unsatCore = ans.highLevelCore().a;
                A4Solution a4 = ans;
                Command counterExample = command;
                List<Pos> removed = new ArrayList<Pos>();
                State save = saveState(sigs);

                // Without this check, the highLevelCore can return gibberish.
                // Learned the hard way.
                while (reporter.minimizedAfter > 0) {
                    reporter.minimizedBefore = 0;
                    reporter.minimizedAfter = 0;

                    Pos constraint = a4.highLevelCore().a.iterator().next();
                    Command newCounterExample = removeGlobalConstraint(constraint, counterExample);
                    if (newCounterExample == null) {
                        if (!removeLocalConstraint(constraint, sigs)) {
                            throw new AlloyIGException("Cannot remove constraint " + constraint);
                        }
                    } else {
                        counterExample = newCounterExample;
                    }
                    removed.add(constraint);
                    a4 = TranslateAlloyToKodkod.execute_command(reporter, world.getAllReachableSigs(), counterExample, options);
                }
                if (removed.isEmpty()) {
                    writeMessage("False");
                } else {
                    writeMessage("True");
                    writeMessage(unsatCore.size());
                    for (Pos pos : unsatCore) {
                        writeMessage(pos.y); // Line
                        writeMessage(pos.x); // Column
                    }
                    writeMessage(removed.size());
                    for (Pos r : removed) {
                        writeMessage(r.toString());
                    }
                    writeMessage(toXml(a4));
                }

                restoreState(save, sigs);
            }
        }
    }
}
