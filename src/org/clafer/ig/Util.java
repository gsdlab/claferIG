package org.clafer.ig;

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprList;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprUnary;
import edu.mit.csail.sdg.alloy4compiler.ast.Sig;
import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

/**
 *
 * @author jimmy
 */
public class Util {

    private static final BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
    private static final PrintStream output = System.out;
    private static final Field isOne;
    private static final Field isLone;
    private static final Field isSome;
    private static final Field facts;

    static {
        try {
            isOne = Sig.class.getDeclaredField("isOne");
            isLone = Sig.class.getDeclaredField("isLone");
            isSome = Sig.class.getDeclaredField("isSome");
            facts = Sig.class.getDeclaredField("facts");

            isOne.setAccessible(true);
            isLone.setAccessible(true);
            isSome.setAccessible(true);
            facts.setAccessible(true);
        } catch (NoSuchFieldException e) {
            throw new AlloyIGException(e);
        }
    }

    private static void set(Sig sig, Field field, Object value) {
        try {
            field.set(sig, value);
        } catch (IllegalAccessException e) {
            throw new AlloyIGException(e);
        }
    }

    public static <T> T notNull(T t) {
        if (t == null) {
            throw new NullPointerException();
        }
        return t;
    }

    public static class State {

        private final SafeList<Expr>[] save;

        private State(SafeList[] save) {
            this.save = save;
        }
    }

    public static State saveState(SafeList<Sig> sigs) {
        SafeList[] save = new SafeList[sigs.size()];
        for (int i = 0; i < save.length; i++) {
            save[i] = sigs.get(i).getFacts();
        }
        return new State(save);
    }

    public static void restoreState(State state, SafeList<Sig> sigs) {
        for (int i = 0; i < state.save.length; i++) {
            set(sigs.get(i), facts, state.save[i]);
        }
    }

    public static String readMessage() throws IOException {
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

    public static void writeMessage(int message) throws IOException {
        writeMessage(Integer.toString(message));
    }

    public static void writeMessage(String message) throws IOException {
        output.println(message.length());
        output.print(message);
    }

    public static Sig findSig(String name, Iterable<Sig> sigs) {
        for (Sig sig : sigs) {
            if (name.equals(sig.label)) {
                return sig;
            }
        }
        throw new AlloyIGException("Unknown sig " + name);
    }

    public static Command removeGlobalConstraint(Pos pos, Command c) {
        Expr newFormula = removeSubnode(pos, c.formula);
        if (notNull(newFormula) == c.formula) {
            return null;
        }

        return new Command(c.pos, c.label, c.check, c.overall, c.bitwidth, c.maxseq, c.expects, c.scope, c.additionalExactScopes, newFormula, c.parent);
    }

    private static <T> List<T> asList(Iterable<T> iterable) {
        List<T> list = new ArrayList<T>();
        for (T item : iterable) {
            list.add(item);
        }
        return list;
    }

    public static boolean removeLocalConstraint(Pos pos, Iterable<Sig> sigs) {
        for (Sig sig : sigs) {
            List<Expr> newFacts = new ArrayList<Expr>(asList(sig.getFacts()));
            for (ListIterator<Expr> iter = newFacts.listIterator(); iter.hasNext();) {
                Expr fact = iter.next();
                // Base case
                if (pos.equals(fact.span())) {
                    iter.remove();

                    set(sig, facts, new SafeList<Expr>(newFacts));
                    return true;
                }

                // Recursive case
                Expr newFact = removeSubnode(pos, fact);
                if (newFact == null) {
                    iter.remove();

                    set(sig, facts, new SafeList<Expr>(newFacts));
                    return true;
                }
                if (newFact != fact) {
                    iter.set(newFact);

                    set(sig, facts, new SafeList<Expr>(newFacts));
                    return true;
                }
            }
        }
        return false;
    }

    private static Expr removeSubnode(Pos pos, Expr node) {
        if (node instanceof ExprList) {
            ExprList exprList = (ExprList) node;

            List<Expr> newSubNodes = new ArrayList<Expr>(exprList.args);
            for (ListIterator<Expr> iter = newSubNodes.listIterator(); iter.hasNext();) {
                Expr next = iter.next();
                // Base case
                if (pos.equals(next.span())) {
                    iter.remove();

                    return ExprList.make(exprList.pos, exprList.closingBracket, exprList.op, newSubNodes);
                }

                // Recursive case
                Expr newSubNode = removeSubnode(pos, next);
                if (newSubNode == null) {
                    iter.remove();

                    return ExprList.make(exprList.pos, exprList.closingBracket, exprList.op, newSubNodes);
                } else if (newSubNode != next) {
                    iter.set(newSubNode);

                    return ExprList.make(exprList.pos, exprList.closingBracket, exprList.op, newSubNodes);
                }
            }
        } else if (node instanceof ExprUnary) {
            ExprUnary expr = (ExprUnary) node;
            ExprUnary.Op op = expr.op;
            Expr sub = expr.sub;

            // Read ExprUnary.getSubnodes to see why NOOP is special
            if (op != ExprUnary.Op.NOOP) {
                if (pos.equals(sub.span())) {
                    return null;
                }
            }
            Expr newSub = removeSubnode(pos, sub);
            if (newSub == null) {
                return null;
            }
            if (newSub != sub) {
                return op.make(expr.pos, newSub);
            }
        }
        return node;
    }
}
