# Different Names

Different programming languages sometimes choose different names for similar operations.
If you're new to Roc, you may be searching for a familiar operation and not find it because
that operation (or a similar one) goes by a different name in Roc.

To help with this, here are some Roc operations along with some names found in other languages for similar operations.

<table>
    <thead>
        <tr>
            <th>Roc Name</th>
            <th>Other Names</th>
        </tr>
    </thead>
    <tbody id="different-names-body">
        <tr>
            <td><a href="/builtins/List#walk">List.walk</a></td>
            <td>
                <ul>
                    <li>fold</li>
                    <li>foldl</li>
                    <li>foldLeft</li>
                    <li>fold_left</li>
                    <li>fold-left</li>
                    <li>reduce</li>
                    <li>lreduce</li>
                    <li>array_reduce</li>
                    <li>inject</li>
                    <li>accumulate</li>
                    <li>Aggregate</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#walk_backwards">List.walk_backwards</a></td>
            <td>
                <ul>
                    <li>foldr</li>
                    <li>foldRight</li>
                    <li>fold_right</li>
                    <li>fold-right</li>
                    <li>reduceRight</li>
                    <li>rreduce</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#first">List.first</a></td>
            <td>
                <ul>
                    <li>head</li>
                    <li>get(0)</li>
                    <li>[0]</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#keep_if">List.keep_if</a></td>
            <td>
                <ul>
                    <li>filter</li>
                    <li>select</li>
                    <li>copy_if</li>
                    <li>remove-if-not</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#drop_if">List.drop_if</a></td>
            <td>
                <ul>
                    <li>reject</li>
                    <li>remove_copy_if</li>
                    <li>remove-if</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#join">List.join</a></td>
            <td>
                <ul>
                    <li>flatten</li>
                    <li>flat</li>
                    <li>concat</li>
                    <li>smoosh</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#join_map">List.join_map</a></td>
            <td>
                <ul>
                    <li>concatMap</li>
                    <li>filterMap</li>
                    <li>filter_map</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#keep_oks">List.keep_oks</a></td>
            <td>
                <ul>
                    <li>compact</li>
                    <li>filterMap(identity)</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/Result#try">Result.try</a></td>
            <td>
                <ul>
                    <li>bind</li>
                    <li>flatMap</li>
                    <li>andThen</li>
                    <li>(&gt;&gt;=)</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td><a href="/builtins/List#map2">List.map2 x y Pair</a></td>
            <td>
                <ul>
                    <li>zip</li>
                </ul>
            </td>
        </tr>
    </tbody>
</table>
