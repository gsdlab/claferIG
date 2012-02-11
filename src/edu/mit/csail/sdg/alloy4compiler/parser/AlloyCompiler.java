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
package edu.mit.csail.sdg.alloy4compiler.parser;

import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.parser.CompModule.Open;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

// This class is based off of CompUtil to extend some functionality.
public class AlloyCompiler {

    // A modified version of the same-named method in CompUtil to handle in memory files
    private static CompModule parseRecursively(List<Object> seenDollar, Map<String, String> loaded, Map<String, String> fc, Pos pos, String filename, CompModule root, String prefix, Set<String> thispath, int initialResolution)
            throws Err, IOException {
        if (thispath.contains(filename)) {
            throw new ErrorSyntax(pos,
                    "Circular dependency in module import. The file \"" + (new File(filename)).getName() + "\" is imported infinitely often.");
        }
        thispath.add(filename);
        // No cycle detected so far. So now we parse the file.
        CompModule u = CompParser.alloy_parseStream(seenDollar, loaded, fc, root, 0, filename, prefix, initialResolution);
        if (prefix.length() == 0) {
            root = u;
        }
        // Here, we recursively open the included files
        for (Open x : u.getOpens()) {
            String cp = (Util.jarPrefix() + "models/" + x.filename + ".als").replace('/', File.separatorChar);
            String content = Util.readAll(cp);
            loaded.put(cp, content);
            CompModule y = parseRecursively(seenDollar, loaded, fc, x.pos, cp, root, (prefix.length() == 0 ? x.alias : prefix + "/" + x.alias), thispath, initialResolution);
            x.connect(y);
        }
        thispath.remove(filename); // Remove this file from the CYCLE DETECTION LIST.
        return u;
    }

    public static CompModule parse(A4Reporter reporter, String filename) throws IOException, Err {
        Map<String, String> fc = new LinkedHashMap<String, String>();
        fc.put("", filename);
        List<Object> seenDollar = new ArrayList<Object>();
        CompModule root = parseRecursively(
                seenDollar,
                new LinkedHashMap<String, String>(),
                fc,
                new Pos("claferIG", 1, 1),
                "",
                null,
                "",
                new LinkedHashSet<String>(),
                1);
        root.seenDollar = seenDollar.size() > 0;
        return CompModule.resolveAll(reporter, root);
    }
}
