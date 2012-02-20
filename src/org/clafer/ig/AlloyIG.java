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
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.CommandScope;
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
import java.util.List;

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

    private static void writeMessage(String message) throws IOException {
        output.println(message.length());
        output.print(message);
    }
    // Alloy4 sends diagnostic messages and progress reports to the A4Reporter.
    // By default, the A4Reporter ignores all these events (but you can extend the A4Reporter to display the event for the user)
    private static final A4Reporter rep = new A4Reporter() {

        @Override
        public void warning(ErrorWarning msg) {
            System.err.print("Relevance Warning:\n" + (msg.toString().trim()) + "\n\n");
            System.err.flush();
        }
    };

    private static interface Operation {
    }

    private static class ResolveOperation implements Operation {
    }

    private static class NextOperation implements Operation {
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

    private static String multiplicity(Sig sig) {
        if(sig.isOne != null) {
            return "One";
        } else if(sig.isLone != null) {
            return "Lone";
        } else if(sig.isSome != null) {
            return "Some";
        }
        return "Any";
    }

    public static void main(String[] args) throws IOException, Err {
        try {
            run(args);
        } catch (EOFException e) {
            System.err.println("AlloyIG stream closed.");
        }
    }

    public static void run(String[] args) throws IOException, Err {
        String modelVerbatim = readMessage();

        // Parse+typecheck the model
        CompModule world = AlloyCompiler.parse(rep, modelVerbatim);
        SafeList<Sig> sigs = world.getAllSigs();

        // Choose some default options for how you want to execute the commands
        A4Options options = new A4Options();
        options.solver = A4Options.SatSolver.SAT4J;

        Command command = world.getAllCommands().get(0);

        // Send back all the sigs
        writeMessage(Integer.toString(sigs.size()));
        for (Sig sig : sigs) {
            writeMessage(sig.label);
            writeMessage(multiplicity(sig));
        }
        // Send back the global scope
        writeMessage(Integer.toString(command.overall));

        A4Solution ans = null;
        Operation operation = new ResolveOperation();

        while (!(operation instanceof QuitOperation)) {
            if (operation instanceof ResolveOperation) {
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
            }

            operation = nextOperation();
        }
    }
}
