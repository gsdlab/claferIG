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
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.CommandScope;
import edu.mit.csail.sdg.ast.Decl;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.ExprConstant;
import edu.mit.csail.sdg.ast.ExprQt;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.parser.CompUtil;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.translator.A4Options;
import edu.mit.csail.sdg.translator.A4Solution;
import edu.mit.csail.sdg.translator.A4Tuple;
import edu.mit.csail.sdg.translator.A4TupleSet;
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod;
import java.io.EOFException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import kodkod.engine.satlab.SATFactory;
import org.alloytools.solvers.natv.minisatprover.FixedMiniSatProver;
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

    private static class LoadOperation implements Operation {

        private final String model;

        public LoadOperation(String model) {
            this.model = model;
        }

        public String getModel() {
            return model;
        }
    }

    private static class ResolveOperation implements Operation {
    }

    private static class NextOperation implements Operation {
    }

    private static class SaveStateOperation implements Operation {
        // To generate counterexamples, we mutate the model. Save
        // the state first and restore the state once completed so
        // the model returns to its original form.
    }

    private static class RestoreStateOperation implements Operation {
    }

    private static class RemoveConstraintOperation implements Operation {

        private final int y, x, y2, x2;

        public RemoveConstraintOperation(int y, int x, int y2, int x2) {
            this.y = y;
            this.x = x;
            this.y2 = y2;
            this.x2 = x2;
        }

        public Pos getPos() {
            return new Pos("", x, y, x2, y2);
        }
    }

    private static class UnsatCoreOperation implements Operation {
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

    private static class SetUnsatCoreMinimizationOperation implements Operation {

        private final int minimizationLevel;

        public SetUnsatCoreMinimizationOperation(int minimizationLevel) {
            if (minimizationLevel < 0 || minimizationLevel > 2) {
                throw new IllegalArgumentException(minimizationLevel + " is in invalid optimization level.");
            }
            this.minimizationLevel = minimizationLevel;
        }

        public int getMinimizationLevel() {
            return minimizationLevel;
        }
    }

    private static class SetBitwidthOperation implements Operation {
        private final int bitwidth;

        public SetBitwidthOperation(int bitwidth) {
            if(bitwidth < 1) {
                throw new IllegalArgumentException(bitwidth + " is an invalid bitwidth.");
            }
            this.bitwidth = bitwidth;
        }

        public int getBitwidth() {
            return bitwidth;
        }
    }

    private static class SetMaxTraceLengthOperation implements Operation {
        private final int maxTraceLength;

        public SetMaxTraceLengthOperation(int maxTraceLength) {
            if (maxTraceLength < 1) {
                throw new IllegalArgumentException(maxTraceLength + " is an invalid trace length.");
            }
            this.maxTraceLength = maxTraceLength;
        }

        public int getMaxTraceLength() {
            return maxTraceLength;
        }
    }

    private static class ForkOperation implements Operation {
        private final int state;

        public ForkOperation(int state) {
            this.state = state;
        }

        public int getState() {
            return state;
        }
    }

    private static class NextConfigurationOperation implements Operation {
    }

    private static Operation nextOperation() throws IOException {
        String op = readMessage();
        if (op == null || op.equals("quit")) {
            return new QuitOperation();
        } else if (op.equals("load")) {
            String model = readMessage();
            return new LoadOperation(model);
        } else if (op.equals("next")) {
            return new NextOperation();
        } else if (op.equals("setGlobalScope")) {
            int scopeSize = readIntMessage();
            return new SetGlobalScopeOperation(scopeSize);
        } else if (op.equals("setScope")) {
            String sig = readMessage();
            int scopeSize = readIntMessage();
            return new SetScopeOperation(sig, scopeSize);
        } else if (op.equals("resolve")) {
            return new ResolveOperation();
        } else if (op.equals("saveState")) {
            return new SaveStateOperation();
        } else if (op.equals("restoreState")) {
            return new RestoreStateOperation();
        } else if (op.equals("removeConstraint")) {
            // The second column is one less than it should be.
            // Not sure if Alloy bug or intended.
            return new RemoveConstraintOperation(readIntMessage(), readIntMessage(), readIntMessage(), readIntMessage() - 1);
        } else if (op.equals("unsatCore")) {
            return new UnsatCoreOperation();
        } else if (op.equals("unsatCoreMinimization")) {
            int optimizationLEvel = readIntMessage();
            return new SetUnsatCoreMinimizationOperation(optimizationLEvel);
        } else if (op.equals("setBitwidth")) {
            int bitwidth = readIntMessage();
            return new SetBitwidthOperation(bitwidth);
        } else if (op.equals("setMaxTraceLength")) {
            return new SetMaxTraceLengthOperation(readIntMessage());
        } else if (op.equals("fork")) {
            return new ForkOperation(readIntMessage());
        } else if (op.equals("nextConfiguration")) {
            return new NextConfigurationOperation();
        }
        throw new AlloyIGException("Unknown op " + op);
    }

    private static CommandScope setCommandScopeSize(int scopeSize, CommandScope cs) throws ErrorSyntax {
        return new CommandScope(
                cs.pos, cs.sigPos, cs.sig, cs.isExact, scopeSize, scopeSize, cs.increment);
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
        ans.writeXML(new PrintWriter(xml), Collections.<Func>emptyList(), null);
        return xml.toString();
    }

    // A dynamic model has mutable sigs or fields; its solutions are traces.
    private static boolean isDynamic(SafeList<Sig> sigs) {
        for (Sig sig : sigs) {
            if (sig.isVariable != null) {
                return true;
            }
            for (Sig.Field field : sig.getFields()) {
                if (field.isVariable != null) {
                    return true;
                }
            }
        }
        return false;
    }

    // The relations of the instance in a state of a trace: the containment relations of the
    // clafers (r_c0_name) and their references (c0_name_ref).
    private static List<Sig.Field> stateFields(Iterable<Sig> sigs) {
        List<Sig.Field> fields = new ArrayList<Sig.Field>();
        for (Sig sig : sigs) {
            for (Sig.Field field : sig.getFields()) {
                if (field.label.startsWith("r_") || field.label.endsWith("_ref")) {
                    fields.add(field);
                }
            }
        }
        return fields;
    }

    // The expression applied n times.
    private static Expr repeat(Expr e, int n, boolean prime) {
        for (int i = 0; i < n; i++) {
            e = prime ? e.prime() : e.after();
        }
        return e;
    }

    // The expression of an atom: the sig of a one sig, a number, or a variable for the atom.
    private static Expr atom(String label, Sig.PrimSig sig, Map<String, Decl> vars) throws Err {
        if (sig.isOne != null) {
            return sig;
        }
        if (sig.builtin) {
            return ExprConstant.makeNUMBER(Integer.parseInt(label));
        }
        Decl decl = vars.get(label);
        if (decl == null) {
            decl = sig.oneOf("atom" + vars.size());
            vars.put(label, decl);
        }
        return decl.get();
    }

    // The relations of a configuration of a dynamic model, which do not change over time.
    private static List<Sig.Field> staticFields(List<Sig.Field> fields) {
        List<Sig.Field> statics = new ArrayList<Sig.Field>();
        for (Sig.Field field : fields) {
            if (field.isVariable == null) {
                statics.add(field);
            }
        }
        return statics;
    }

    // Some distinct atoms make the given relations in the first states of the trace equal to those
    // of the given trace, up to a renaming of its atoms; with the loop, from the loop state, the
    // trace repeats the loop, so that it is the given trace.
    private static Expr sameStates(A4Solution ans, List<Sig.Field> fields, int count, boolean withLoop) throws Err {
        Map<String, Decl> vars = new LinkedHashMap<String, Decl>();
        int length = ans.getTraceLength();
        int loop = ans.getLoopState();
        Expr trace = ExprConstant.TRUE;
        for (int s = Math.min(count, length) - 1; s >= 0; s--) {
            Expr state = ExprConstant.TRUE;
            for (Sig.Field field : fields) {
                Expr value = null;
                for (A4Tuple tuple : (A4TupleSet) ans.eval(field, s)) {
                    Expr t = null;
                    for (int i = 0; i < tuple.arity(); i++) {
                        Expr a = atom(tuple.atom(i), tuple.sig(i), vars);
                        t = t == null ? a : t.product(a);
                    }
                    value = value == null ? t : value.plus(t);
                }
                state = state.and(value == null ? field.no() : field.equal(value));
            }
            trace = s == Math.min(count, length) - 1 ? state : state.and(trace.after());
        }
        if (withLoop) {
            Expr periodic = ExprConstant.TRUE;
            for (Sig.Field field : fields) {
                periodic = periodic.and(field.equal(repeat(field, length - loop, true)));
            }
            trace = trace.and(repeat(periodic.always(), loop, false));
        }
        List<Decl> decls = new ArrayList<Decl>(vars.values());
        for (int i = 0; i < decls.size(); i++) {
            for (int j = i + 1; j < decls.size(); j++) {
                trace = decls.get(i).get().equal(decls.get(j).get()).not().and(trace);
            }
        }
        return decls.isEmpty() ? trace : ExprQt.Op.SOME.make(Pos.UNKNOWN, Pos.UNKNOWN, decls, trace);
    }

    // The command with the given bounds of the trace length.
    private static Command withTraceLength(Command c, int minTraceLength, int maxTraceLength) {
        return new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, c.bitwidth, c.maxseq, minTraceLength, maxTraceLength, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
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
        // Alloy logs its progress at the info level and warns whenever a native solver is garbage collected.
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "error");
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

        CompModule world = null;
        SafeList<Sig> sigs = null;
        Command command = null;
        // Alloy parses the model from a temporary file; positions only match with its name.
        String modelFile = "";

        A4Solution ans = null;
        // The solution of the model, as solved by the last resolve: if it is unsatisfiable, the
        // model has no instances, and its unsat core explains why.
        A4Solution resolved = null;
        Operation operation = null;
        boolean dynamic = false;
        // In a dynamic model, the traces given so far are excluded from the next solutions, and the
        // configurations given so far from the next configuration. The next trace is solved for
        // when it is asked for, after the trace shown last.
        Expr givenTraces = ExprConstant.TRUE;
        Expr givenConfigurations = ExprConstant.TRUE;
        A4Solution shown = null;
        boolean stale = false;

        A4Options options = new A4Options();
        // Use fastest
        options.coreMinimization = 2;
        // The minisat prover computes unsat cores. Alloy has none for Windows, so instances come from SAT4J there.
        // ClaferIG passes the optimized build of the prover in the system property alloy.native.lib.minisatprover.
        SATFactory prover = new FixedMiniSatProver.Factory();
        options.solver = prover.isPresent() ? prover : SATFactory.get("sat4j");

        State<StateExtra> state = null;

        while (true) {
            operation = nextOperation();

            if (operation instanceof LoadOperation) {
                LoadOperation load = (LoadOperation) operation;

                world = CompUtil.parseEverything_fromString(rep, load.getModel());
                sigs = world.getAllSigs();
                dynamic = isDynamic(sigs);
                command = world.getAllCommands().get(0);
                modelFile = command.pos.filename;

                // Send back all the sigs
                writeMessage(Integer.toString(sigs.size()));
                for (Sig sig : sigs) {
                    writeMessage(sig.label);
                    writeMessage(multiplicity(sig));
                    writeMessage(sig instanceof Sig.PrimSig ? "" : removeCurly(sig.type().toString()));
                    CommandScope scope = command.getScope(sig);
                    if (scope == null) {
                        writeMessage("False");
                    } else {
                        writeMessage("True");
                        writeMessage(scope.startingScope);
                    }
                }
                
                // Send back the global scope
                writeMessage(Integer.toString(command.overall));
            } else if (operation instanceof ResolveOperation) {
                rep.minimizedBefore = 0;
                rep.minimizedAfter = 0;
                givenTraces = ExprConstant.TRUE;
                givenConfigurations = ExprConstant.TRUE;
                shown = null;
                stale = false;
                // Reexecute the command
                ans = TranslateAlloyToKodkod.execute_command(rep, world.getAllReachableSigs(), command, options);
                resolved = ans;
            } else if (operation instanceof NextOperation) {
                if (dynamic && stale) {
                    // Alloy does not enumerate the traces of a temporal model: it gives few of them,
                    // of the length it found first, or none at all, depending on the model. Instead,
                    // the next trace is solved for, without the traces given so far. The reporter of
                    // the model stays as it is, since no more traces is not an unsat core of the model.
                    ans = TranslateAlloyToKodkod.execute_command(new AlloyIGReporter(), world.getAllReachableSigs(),
                            command.change(command.formula.and(givenTraces)), options);
                    stale = false;
                }
                if (ans.satisfiable()) {
                    writeMessage("True");
                    writeMessage(toXml(ans));

                    if (dynamic) {
                        List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
                        givenTraces = givenTraces.and(sameStates(ans, fields, ans.getTraceLength(), true).not());
                        givenConfigurations = givenConfigurations.and(sameStates(ans, staticFields(fields), 1, false).not());
                        shown = ans;
                        stale = true;
                    } else {
                        A4Solution nextAns = ans.next();
                        if (nextAns == ans) {
                            break;
                        }
                        ans = nextAns;
                    }
                } else {
                    writeMessage("False");
                }
            } else if (operation instanceof ForkOperation || operation instanceof NextConfigurationOperation) {
                // A fork of the trace shown last at a state: a trace with the same states before
                // it. A new configuration: a trace whose relations that do not change over time
                // differ from those of the traces given so far. Both differ from the traces given
                // so far.
                A4Solution other = null;
                if (dynamic && shown != null) {
                    List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
                    Expr formula = operation instanceof ForkOperation
                            ? sameStates(shown, fields, ((ForkOperation) operation).getState(), false)
                            : givenConfigurations;
                    other = TranslateAlloyToKodkod.execute_command(new AlloyIGReporter(), world.getAllReachableSigs(),
                            command.change(command.formula.and(givenTraces).and(formula)), options);
                }
                if (other != null && other.satisfiable()) {
                    writeMessage("True");
                    writeMessage(toXml(other));
                    List<Sig.Field> fields = stateFields(world.getAllReachableSigs());
                    givenTraces = givenTraces.and(sameStates(other, fields, other.getTraceLength(), true).not());
                    givenConfigurations = givenConfigurations.and(sameStates(other, staticFields(fields), 1, false).not());
                    shown = other;
                    ans = other;
                    stale = true;
                } else {
                    writeMessage("False");
                }
            } else if (operation instanceof SetGlobalScopeOperation) {
                SetGlobalScopeOperation increaseScope = (SetGlobalScopeOperation) operation;
                int scopeSize = increaseScope.getScopeSize();

                Command c = command;
                command = new Command(c.pos, c.nameExpr, c.label, c.check, scopeSize, c.bitwidth, c.maxseq, c.minprefix, c.maxprefix, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
            } else if (operation instanceof SetScopeOperation) {
                SetScopeOperation increaseScope = (SetScopeOperation) operation;
                String sigName = increaseScope.getSig();
                int scopeSize = increaseScope.getScopeSize();

                Sig sig = findSig(sigName, sigs);
                List<CommandScope> scope = setScopeSize(sig, scopeSize, command.scope);

                Command c = command;
                command = new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, c.bitwidth, c.maxseq, c.minprefix, c.maxprefix, c.expects, scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
            } else if (operation instanceof SaveStateOperation) {
                state = saveState(sigs, new StateExtra(rep.minimizedBefore, rep.minimizedAfter, ans, resolved, command, givenTraces));
            } else if (operation instanceof RestoreStateOperation) {
                StateExtra extra = restoreState(state, sigs);
                rep.minimizedBefore = extra.getMinimizedBefore();
                rep.minimizedAfter = extra.getMinimizedAfter();
                ans = extra.getAnswer();
                resolved = extra.getResolved();
                command = extra.getCommand();
                givenTraces = extra.getGivenTraces();
            } else if (operation instanceof RemoveConstraintOperation) {
                RemoveConstraintOperation removeConstraint = (RemoveConstraintOperation) operation;
                Pos at = removeConstraint.getPos();
                Pos constraint = new Pos(modelFile, at.x, at.y, at.x2, at.y2);

                Command newCommand = removeGlobalConstraint(constraint, command);
                if (newCommand == null) {
                    if (!removeLocalConstraint(constraint, sigs)) {
                        throw new AlloyIGException(
                                String.format("Cannot remove constraint (line=%d, column=%d)-(line=%d, column=%d)",
                                constraint.y, constraint.x, constraint.y2, constraint.x2));
                    }
                } else {
                    command = newCommand;
                }
            } else if (operation instanceof UnsatCoreOperation) {
                // Only the model has an unsat core: not the next solution, when the solutions of the
                // model run out. Alloy reports no minimized core when it finds the model
                // unsatisfiable without solving, e.g., [ some b ] and [ no b ], so the core is not
                // checked by its size.
                if (ans == resolved && !ans.satisfiable()) {
                    Set<Pos> unsatCore = ans.highLevelCore().a;

                    writeMessage(unsatCore.size());
                    for (Pos unsate : unsatCore) {
                        writeMessage(unsate.y); // Line
                        writeMessage(unsate.x); // Column
                        writeMessage(unsate.y2); // Line
                        // The second column is one less than it should be.
                        // Not sure if Alloy bug or intended.
                        writeMessage(unsate.x2 + 1); // Column
                    }
                } else {
                    writeMessage(0);
                }
            } else if(operation instanceof SetUnsatCoreMinimizationOperation) {
                SetUnsatCoreMinimizationOperation setUnsatCoreMinimization = (SetUnsatCoreMinimizationOperation) operation;
                options.coreMinimization = setUnsatCoreMinimization.getMinimizationLevel();
            } else if (operation instanceof SetBitwidthOperation) {
                SetBitwidthOperation setBitwidth = (SetBitwidthOperation) operation;
                Command c = command;
                command = new Command(c.pos, c.nameExpr, c.label, c.check, c.overall, setBitwidth.getBitwidth(), c.maxseq, c.minprefix, c.maxprefix, c.expects, c.scope, c.additionalExactScopes, c.commandKeyword, c.formula, c.parent);
            } else if (operation instanceof SetMaxTraceLengthOperation) {
                int maxTraceLength = ((SetMaxTraceLengthOperation) operation).getMaxTraceLength();
                command = withTraceLength(command, Math.min(command.minprefix, maxTraceLength), maxTraceLength);
            } else if (operation instanceof QuitOperation) {
                break;
            } else {
                throw new IllegalStateException("Unknown operation " + operation);
            }
        }
    }

    private static class StateExtra {

        private final int minimizedBefore;
        private final int minimizedAfter;
        private final A4Solution answer;
        private final A4Solution resolved;
        private final Command command;
        private final Expr givenTraces;

        public StateExtra(int minimizedBefore, int minimizedAfter, A4Solution answer, A4Solution resolved, Command command, Expr givenTraces) {
            this.minimizedBefore = minimizedBefore;
            this.minimizedAfter = minimizedAfter;
            this.answer = notNull(answer);
            this.resolved = resolved;
            this.command = notNull(command);
            this.givenTraces = notNull(givenTraces);
        }

        public A4Solution getResolved() {
            return resolved;
        }

        public Expr getGivenTraces() {
            return givenTraces;
        }

        public int getMinimizedBefore() {
            return minimizedBefore;
        }

        public int getMinimizedAfter() {
            return minimizedAfter;
        }

        public A4Solution getAnswer() {
            return answer;
        }

        public Command getCommand() {
            return command;
        }
    }
}
