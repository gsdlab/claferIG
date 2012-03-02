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
import edu.mit.csail.sdg.alloy4compiler.ast.Browsable;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.CommandScope;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprList;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import edu.mit.csail.sdg.alloy4compiler.parser.AlloyCompiler;
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Options;
import edu.mit.csail.sdg.alloy4compiler.translator.A4Solution;
import edu.mit.csail.sdg.alloy4compiler.translator.TranslateAlloyToKodkod;
import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public final class AlloyIG {

    private static BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
    private static PrintStream output = System.out;

    private static String readMessage() throws IOException {
        String line = input.readLine();
        if (line == null) {
            throw new EOFException();
        }
        int length = Integer.parseInt(line);

        char[] buf = new char[length];
        int off = 0;

        while (off < length) {
            int l = input.read(buf, off, length - off);
            if (l == -1) {
                throw new EOFException();
            }

            off += l;
        }

        return new String(buf);
    }

    private static void writeMessage(int message) throws IOException {
        writeMessage(Integer.toString(message));
    }

    private static void writeMessage(String message) throws IOException {
        output.println(message.length());
        output.print(message);
    }

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

    private static Pair<Pos, Command> removeConstraint(Set<Pos> core, Set<Pos> subcore, Command command) {
        for (Pos pos : core) {
            Command newCommand = removeConstraint(pos, command);
            if (newCommand != null) {
                return new Pair<Pos, Command>(pos, newCommand);
            }
        }
        for (Pos pos : subcore) {
            Command newCommand = removeConstraint(pos, command);
            if (newCommand != null) {
                return new Pair<Pos, Command>(pos, newCommand);
            }
        }
        return null;
    }

    private static Command removeConstraint(Pos pos, Command c) {
        ExprList formula = (ExprList) c.formula;
        List<Expr> newSubnodes = removeSubnode(pos, formula.args);
        if (newSubnodes == null) {
            return null;
        }
        ExprList newFormula = ExprList.make(formula.pos, formula.closingBracket, formula.op, newSubnodes);

        return new Command(c.pos, c.label, c.check, c.overall, c.bitwidth, c.maxseq, c.expects, c.scope, c.additionalExactScopes, newFormula, c.parent);
    }

    private static List<Expr> removeSubnode(Pos pos, List<Expr> subnodes) {
        List<Expr> result = new ArrayList<Expr>(subnodes);
        Iterator<Expr> iter = result.iterator();
        while (iter.hasNext()) {
            Browsable next = iter.next();
            if (pos.equals(next.span())) {
                iter.remove();
                return result;
            }
        }
        return null;
    }

    private static interface Operation {
    }

    private static class ResolveOperation implements Operation {
    }

    private static class NextOperation implements Operation {
    }

    private static class UnsatCoreOperation implements Operation {
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
        } else if (op.equals("unsatCore")) {
            return new UnsatCoreOperation();
        } else if (op.equals("counterexample")) {
            return new CounterexampleOperation();
        }
        throw new AlloyIGException("Unknown op " + op);
    }

    private static CommandScope setCommandScopeSize(int scopeSize, CommandScope cs) throws ErrorSyntax {
        return new CommandScope(
                cs.pos, cs.sig, cs.isExact, scopeSize, scopeSize, cs.increment);
    }

    private static Sig findSig(String name, Iterable<Sig> sigs) {
        for (Sig sig : sigs) {
            if (name.equals(sig.label)) {
                return sig;
            }
        }
        throw new AlloyIGException("Unknown sig " + name);
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
            } else if (operation instanceof UnsatCoreOperation) {
                // Without this check, the highLevelCore can return gibberish.
                // Learned the hard way.
                if (rep.minimizedAfter > 0) {
                    Pair<Set<Pos>, Set<Pos>> unsatCore = ans.highLevelCore();
                    writeMessage(unsatCore.a.size());
                    for (Pos pos : unsatCore.a) {
                        writeMessage(pos.toString());
                    }
                    writeMessage(unsatCore.b.size());
                    for (Pos pos : unsatCore.b) {
                        writeMessage(pos.toString());
                    }
                } else {
                    writeMessage(0);
                    writeMessage(0);
                }
            } else if (operation instanceof CounterexampleOperation) {
                AlloyIGReporter reporter = new AlloyIGReporter();
                reporter.minimizedBefore = rep.minimizedBefore;
                reporter.minimizedAfter = rep.minimizedAfter;

                // Without this check, the highLevelCore can return gibberish.
                // Learned the hard way.
                if (reporter.minimizedAfter > 0) {
                    A4Solution a4 = ans;
                    Command counterExample = command;
                    List<Pos> removed = new ArrayList<Pos>();
                    do {
                        reporter.minimizedBefore = 0;
                        reporter.minimizedAfter = 0;

                        Pair<Pos, Command> removedPair = removeConstraint(a4.highLevelCore().a, a4.highLevelCore().b, counterExample);
                        if (removedPair == null) {
                            writeMessage("False");
                            removed = null;
                            break;
                        }
                        removed.add(removedPair.a);
                        counterExample = removedPair.b;
                        a4 = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), counterExample, options);
                    } while (reporter.minimizedAfter > 0);
                    if (removed != null) {
                        writeMessage("True");
                        writeMessage(removed.size());
                        for (Pos r : removed) {
                            writeMessage(r.toString());
                        }
                        writeMessage(toXml(a4));
                    }
                } else {
                    writeMessage("False");
                }
            }
        }
    }
}
