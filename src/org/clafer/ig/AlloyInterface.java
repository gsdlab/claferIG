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
import edu.mit.csail.sdg.alloy4.ErrorWarning;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
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

public final class AlloyInterface {

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

    private static class NextOperation implements Operation {
    }

    private static class QuitOperation implements Operation {
    }

    private static class SigsOperation implements Operation {
    }

    private static class IncreaseScopeOperation implements Operation {

        private final int increment;

        public IncreaseScopeOperation(int increment) {
            this.increment = increment;
        }

        public int getIncrement() {
            return increment;
        }
    }

    private static Operation nextOperation() throws IOException {
        String op = readMessage();
        if (op == null || op.equals("q")) {
            return new QuitOperation();
        } else if (op.equals("n")) {
            return new NextOperation();
        } else if (op.equals("s")) {
            return new SigsOperation();
        } else if (op.equals("i")) {
            int increment = Integer.parseInt(readMessage());
            return new IncreaseScopeOperation(increment);
        }
        throw new IOException("Unknown op " + op);
    }

    public static void main(String[] args) throws IOException, Err {
        String modelVerbatim = readMessage();

        // Parse+typecheck the model
        CompModule world = AlloyCompiler.parse(rep, modelVerbatim);

        // Choose some default options for how you want to execute the commands
        A4Options options = new A4Options();
        options.solver = A4Options.SatSolver.MiniSatProverJNI;

        Command command = world.getAllCommands().get(0);
        // Execute the command
        A4Solution ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);

        while (true) {
            Operation operation = nextOperation();

            if (operation instanceof NextOperation) {
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
            } else if (operation instanceof SigsOperation) {
                SafeList<Sig> sigs = world.getAllSigs();
                writeMessage(Integer.toString(sigs.size()));
                for (Sig sig : sigs) {
                    writeMessage(sig.label);
                }
            } else if (operation instanceof IncreaseScopeOperation) {
                IncreaseScopeOperation increaseScope = (IncreaseScopeOperation) operation;
                int increment = increaseScope.getIncrement();

                Command c = command;
                command = new Command(c.pos, c.label, c.check, c.overall + increment, c.bitwidth, c.maxseq, c.expects, c.scope, c.additionalExactScopes, c.formula, c.parent);

                writeMessage(Integer.toString(command.overall));

                // Reexecute the command
                ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
                
            } else if (operation instanceof QuitOperation) {
                break;
            }
        }
    }
}
