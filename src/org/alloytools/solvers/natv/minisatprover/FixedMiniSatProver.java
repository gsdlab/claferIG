/*
 * Copyright (C) 2026 Michal Antkiewicz <http://gsd.uwaterloo.ca>
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
package org.alloytools.solvers.natv.minisatprover;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.SATSolver;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import java.util.Iterator;

/**
 * The minisat prover of Alloy 6.2.0 with working unsat cores.
 *
 * Alloy's MiniSatProver frees the native solver in proof() and reduce() before it
 * reads the number of variables and clauses, which then throws. Alloy ignores the
 * exception, so every unsat core is empty. This class delegates the solving to
 * MiniSatProver and reads the counts before freeing. It is in Alloy's package to use
 * the package-private LazyTrace and MiniSatProver.trace.
 */
public final class FixedMiniSatProver implements SATProver {

    /**
     * Factory for FixedMiniSatProver; otherwise the same as Alloy's minisat.prover.
     */
    public static final class Factory extends MiniSatProverRef {

        private static final long serialVersionUID = 1L;

        @Override
        public SATSolver createSolver() {
            return new FixedMiniSatProver();
        }
    }

    private final MiniSatProver solver = new MiniSatProver();
    private Boolean sat = null;
    private LazyTrace proof = null;
    private int variables;
    private int clauses;

    @Override
    public int numberOfVariables() {
        return proof == null ? solver.numberOfVariables() : variables;
    }

    @Override
    public int numberOfClauses() {
        return proof == null ? solver.numberOfClauses() : clauses;
    }

    @Override
    public void addVariables(int numVars) {
        solver.addVariables(numVars);
    }

    @Override
    public boolean addClause(int[] lits) {
        return solver.addClause(lits);
    }

    @Override
    public boolean solve() {
        sat = solver.solve();
        return sat;
    }

    @Override
    public boolean valueOf(int variable) {
        return solver.valueOf(variable);
    }

    @Override
    public void free() {
        solver.free();
    }

    @Override
    public ResolutionTrace proof() {
        if (!Boolean.FALSE.equals(sat)) {
            throw new IllegalStateException();
        }
        if (proof == null) {
            variables = solver.numberOfVariables();
            clauses = solver.numberOfClauses();
            int[][] trace = solver.trace(solver.peer(), true);
            solver.free();
            proof = trace[trace.length - 1].length == 0
                    ? new LazyTrace(formatTrivial(trace), clauses)
                    : new LazyTrace(format(trace), clauses);
        }
        return proof;
    }

    @Override
    public void reduce(ReductionStrategy strategy) {
        proof();
        if (proof.resolvents().isEmpty()) {
            return;
        }
        for (IntSet next = strategy.next(proof); !next.isEmpty(); next = strategy.next(proof)) {
            MiniSatProver reduced = new MiniSatProver();
            try {
                reduced.addVariables(variables);
                for (Iterator<Clause> itr = proof.iterator(next); itr.hasNext();) {
                    if (!reduced.addClause(itr.next().toArray())) {
                        throw new AssertionError("Clause rejected while reducing the unsat core.");
                    }
                }
                if (!reduced.solve()) {
                    clauses = next.size();
                    int[][] trace = reduced.trace(reduced.peer(), false);
                    proof = new LazyTrace(proof, next, format(trace));
                }
            } finally {
                reduced.free();
            }
        }
    }

    // The same as MiniSatProver.formatTrivial, with the stored number of variables.
    private int[][] formatTrivial(int[][] trace) {
        int length = trace.length;
        int conflict = length - 1;
        IntSet relevant = new IntBitSet(length);
        relevant.add(conflict);
        for (int i = 0; i < conflict; i++) {
            if (trace[i][0] <= variables) {
                relevant.add(i);
            }
        }
        if (relevant.size() == length) {
            return trace;
        }
        int[][] formatted = new int[relevant.size()][];
        int i = 0;
        for (IntIterator itr = relevant.iterator(); itr.hasNext();) {
            formatted[i++] = trace[itr.next()];
        }
        return formatted;
    }

    // The same as MiniSatProver.format, with the stored number of variables.
    private int[][] format(int[][] trace) {
        int length = trace.length;
        IntSet resolvents = new IntBitSet(length);
        int offset = variables + 1;
        for (int i = 0; i < length; i++) {
            int[] clause = trace[i];
            if (clause != null && clause[0] >= offset) {
                clause[0] -= offset;
                resolvents.add(i);
            }
        }
        int axioms = length - resolvents.size();
        if (resolvents.min() < axioms) {
            int[] position = new int[length];
            for (int i = 0, axiomIndex = 0, resolventIndex = axioms; i < length; i++) {
                if (resolvents.contains(i)) {
                    position[i] = resolventIndex++;
                    int[] resolvent = trace[i];
                    for (int j = 0, max = resolvent.length; j < max; j++) {
                        resolvent[j] = position[resolvent[j]];
                    }
                } else {
                    position[i] = axiomIndex++;
                }
            }
            for (IntIterator itr = resolvents.iterator(length, 0); itr.hasNext();) {
                int i = itr.next();
                int pos = position[i];
                if (i == pos) {
                    continue;
                }
                int[] resolvent = trace[i];
                System.arraycopy(trace, i + 1, trace, i, pos - i);
                trace[pos] = resolvent;
            }
        }
        return trace;
    }
}
