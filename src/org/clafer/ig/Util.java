package org.clafer.ig;

import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.SafeList;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.ast.ExprLet;
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

    public static class State<T> {

        private final SafeList<Expr>[] save;
        private final Pos[] isOnes;
        private final Pos[] isLones;
        private final Pos[] isSomes;
        private final T extra;

        public State(SafeList<Expr>[] save, Pos[] isOnes, Pos[] isLones, Pos[] isSomes, T extra) {
            this.save = save;
            this.isOnes = isOnes;
            this.isLones = isLones;
            this.isSomes = isSomes;
            this.extra = extra;
        }
    }

    public static <T> State<T> saveState(SafeList<Sig> sigs, T extra) {
        SafeList[] save = new SafeList[sigs.size()];
        Pos[] isOnes = new Pos[sigs.size()];
        Pos[] isLones = new Pos[sigs.size()];
        Pos[] isSomes = new Pos[sigs.size()];
        for (int i = 0; i < save.length; i++) {
            Sig sig = sigs.get(i);
            save[i] = sig.getFacts();
            isOnes[i] = sig.isOne;
            isLones[i] = sig.isLone;
            isSomes[i] = sig.isSome;
        }
        
        return new State(save, isOnes, isLones, isSomes, extra);
    }

    public static <T> T restoreState(State<T> state, SafeList<Sig> sigs) {
        for (int i = 0; i < state.save.length; i++) {
            Sig sig = sigs.get(i);
            set(sig, facts, state.save[i]);
            set(sig, isOne, state.isOnes[i]);
            set(sig, isLone, state.isLones[i]);
            set(sig, isSome, state.isSomes[i]);
        }
        return state.extra;
    }

    public static int readIntMessage() throws IOException {
        return Integer.parseInt(readMessage());
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
            if(pos.equals(sig.isOne)) {
                set(sig, isOne, null);
                return true;
            }
            if(pos.equals(sig.isLone)) {
                set(sig, isLone, null);
                return true;
            }
            if(pos.equals(sig.isSome)) {
                set(sig, isSome, null);
                return true;
            }
            List<Expr> newFacts = new ArrayList<Expr>(asList(sig.getFacts()));
            for (ListIterator<Expr> iter = newFacts.listIterator(); iter.hasNext();) {
                Expr fact = iter.next();

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
        if (pos.equals(node.span())) {
            return null;
        }
        if (node instanceof ExprList) {
            ExprList exprList = (ExprList) node;

            List<Expr> newSubNodes = new ArrayList<Expr>(exprList.args);
            for (ListIterator<Expr> iter = newSubNodes.listIterator(); iter.hasNext();) {
                Expr next = iter.next();

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

            Expr newSub = removeSubnode(pos, sub);
            if (newSub == null) {
                return null;
            }
            if (newSub != sub) {
                return op.make(expr.pos, newSub);
            }
        } else if (node instanceof ExprLet) {
            ExprLet let = (ExprLet) node;

            Expr newSub = removeSubnode(pos, let.sub);
            if (newSub == null) {
                return null;
            }
            if (newSub != let.sub) {
                ExprLet.make(let.pos, let.var, let.expr, newSub);
            }
        }
        return node;
    }
}
