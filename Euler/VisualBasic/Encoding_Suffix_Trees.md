# Euler Problem: Encoding Suffix Trees in Visual Basic

## Problem Understanding

The problem requires implementing a suffix tree data structure in Visual Basic. A suffix tree is a compressed trie containing all suffixes of a given text, which is useful for string matching and pattern searching operations.

## Solution Implementation

```vb
Public Class SuffixTreeNode
    Public Property Children As Dictionary(Of Char, SuffixTreeNode)
    Public Property Start As Integer
    Public Property End As Integer
    Public Property SuffixIndex As Integer
    
    Public Sub New()
        Children = New Dictionary(Of Char, SuffixTreeNode)()
        SuffixIndex = -1
    End Sub
End Class

Public Class SuffixTree
    Private _root As SuffixTreeNode
    Private _text As String
    Private _textLength As Integer
    Private _activeNode As SuffixTreeNode
    Private _activeEdge As Integer
    Private _activeLength As Integer
    Private _remainder As Integer
    Private _lastNewNode As SuffixTreeNode
    
    Public Sub New(text As String)
        _root = New SuffixTreeNode()
        _text = text
        _textLength = text.Length
        _activeNode = _root
        _activeEdge = -1
        _activeLength = 0
        _remainder = 0
        _lastNewNode = Nothing
        
        ' Build the suffix tree
        BuildSuffixTree()
    End Sub
    
    Private Sub BuildSuffixTree()
        Dim i As Integer = 0
        For i = 0 To _textLength - 1
            ExtendSuffixTree(i)
        Next
    End Sub
    
    Private Sub ExtendSuffixTree(pos As Integer)
        _remainder += 1
        _lastNewNode = Nothing
        
        While _remainder > 0
            If _activeLength = 0 Then
                _activeEdge = pos
            End If
            
            If Not _activeNode.Children.ContainsKey(_text(_activeEdge)) Then
                ' Create new leaf node
                Dim leaf As New SuffixTreeNode()
                leaf.Start = pos
                leaf.End = _textLength - 1
                leaf.SuffixIndex = pos - _textLength + 1
                
                _activeNode.Children.Add(_text(_activeEdge), leaf)
                
                ' Check for suffix link
                If _lastNewNode IsNot Nothing AndAlso _activeNode IsNot _root Then
                    _lastNewNode.SuffixIndex = _activeNode.SuffixIndex
                    _lastNewNode = Nothing
                End If
            Else
                Dim nextNode As SuffixTreeNode = _activeNode.Children(_text(_activeEdge))
                Dim edgeLength As Integer = nextNode.End - nextNode.Start + 1
                
                If _activeLength >= edgeLength Then
                    ' Move to next node
                    _activeEdge += edgeLength
                    _activeLength -= edgeLength
                    _activeNode = nextNode
                    Continue While
                Else
                    ' Check if the character matches
                    If _text(nextNode.Start + _activeLength) = _text(pos) Then
                        ' Character matches, increment active length
                        _activeLength += 1
                        
                        ' Check for suffix link
                        If _lastNewNode IsNot Nothing AndAlso _activeNode IsNot _root Then
                            _lastNewNode.SuffixIndex = _activeNode.SuffixIndex
                            _lastNewNode = Nothing
                        End If
                        
                        Exit While
                    Else
                        ' Split the edge
                        Dim splitNode As New SuffixTreeNode()
                        splitNode.Start = nextNode.Start
                        splitNode.End = nextNode.Start + _activeLength - 1
                        
                        ' Add new leaf
                        Dim leaf As New SuffixTreeNode()
                        leaf.Start = pos
                        leaf.End = _textLength - 1
                        leaf.SuffixIndex = pos - _textLength + 1
                        
                        ' Add new internal node
                        Dim newNode As New SuffixTreeNode()
                        newNode.Start = nextNode.Start + _activeLength
                        newNode.End = nextNode.End
                        
                        ' Update the original node
                        nextNode.Start += _activeLength
                        nextNode.SuffixIndex = -1
                        
                        ' Add children
                        splitNode.Children.Add(_text(splitNode.Start + _activeLength), newNode)
                        splitNode.Children.Add(_text(pos), leaf)
                        
                        ' Update parent
                        If _activeNode.Children.ContainsKey(_text(_activeEdge)) Then
                            _activeNode.Children(_text(_activeEdge)) = splitNode
                        End If
                        
                        ' Set suffix link
                        If _lastNewNode IsNot Nothing Then
                            _lastNewNode.SuffixIndex = splitNode.SuffixIndex
                        End If
                        
                        _lastNewNode = splitNode
                        
                        ' Continue with remaining suffixes
                        _remainder -= 1
                    End If
                End If
            End If
        End While
    End Sub
    
    ' Search for a pattern in the suffix tree
    Public Function Search(pattern As String) As Boolean
        Dim currentNode As SuffixTreeNode = _root
        Dim i As Integer = 0
        
        While i < pattern.Length
            If Not currentNode.Children.ContainsKey(pattern(i)) Then
                Return False
            End If
            
            Dim nextNode As SuffixTreeNode = currentNode.Children(pattern(i))
            Dim edgeLength As Integer = nextNode.End - nextNode.Start + 1
            
            Dim j As Integer = 0
            While j < edgeLength AndAlso i + j < pattern.Length
                If _text(nextNode.Start + j) <> pattern(i + j) Then
                    Return False
                End If
                j += 1
            End While
            
            i += j
            currentNode = nextNode
        End While
        
        Return True
    End Function
    
    ' Get all suffixes from the tree
    Public Function GetAllSuffixes() As List(Of String)
        Dim suffixes As New List(Of String)()
        CollectSuffixes(_root, "", suffixes)
        Return suffixes
    End Function
    
    Private Sub CollectSuffixes(node As SuffixTreeNode, currentSuffix As String, suffixes As List(Of String))
        If node.SuffixIndex >= 0 Then
            suffixes.Add(currentSuffix)
        End If
        
        For Each kvp As KeyValuePair(Of Char, SuffixTreeNode) In node.Children
            Dim child As SuffixTreeNode = kvp.Value
            Dim edgeText As String = _text.Substring(child.Start, child.End - child.Start + 1)
            CollectSuffixes(child, currentSuffix & edgeText, suffixes)
        Next
    End Sub
    
    ' Print the suffix tree structure
    Public Sub PrintTree()
        Console.WriteLine("Suffix Tree Structure:")
        PrintNode(_root, "", 0)
    End Sub
    
    Private Sub PrintNode(node As SuffixTreeNode, prefix As String, depth As Integer)
        If node.SuffixIndex >= 0 Then
            Console.WriteLine($"{prefix}Leaf: SuffixIndex = {node.SuffixIndex}")
        End If
        
        For Each kvp As KeyValuePair(Of Char, SuffixTreeNode) In node.Children
            Dim child As SuffixTreeNode = kvp.Value
            Dim edgeText As String = _text.Substring(child.Start, child.End - child.Start + 1)
            Console.WriteLine($"{prefix}Edge: {edgeText}")
            PrintNode(child, prefix & "  ", depth + 1)
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim text As String = "banana$"
        Dim suffixTree As New SuffixTree(text)
        
        Console.WriteLine($"Building suffix tree for: {text}")
        suffixTree.PrintTree()
        
        ' Test searching
        Console.WriteLine(vbNewLine & "Searching for patterns:")
        Console.WriteLine($"Pattern 'ana': {suffixTree.Search("ana")}")
        Console.WriteLine($"Pattern 'ban': {suffixTree.Search("ban")}")
        Console.WriteLine($"Pattern 'xyz': {suffixTree.Search("xyz")}")
        
        ' Get all suffixes
        Console.WriteLine(vbNewLine & "All suffixes:")
        Dim suffixes As List(Of String) = suffixTree.GetAllSuffixes()
        For Each suffix As String In suffixes
            Console.WriteLine(suffix)
        Next
    End Sub
End Module
```

## Key Features of the Implementation

1. **SuffixTreeNode Class**: Represents nodes in the suffix tree with:
   - Children dictionary for child nodes
   - Start and end indices for edge labels
   - Suffix index for leaf nodes

2. **SuffixTree Class**: Implements the suffix tree construction algorithm:
   - Uses Ukkonen's algorithm for efficient construction
   - Handles active point management
   - Supports suffix links for optimization

3. **Core Methods**:
   - `BuildSuffixTree()`: Constructs the tree using Ukkonen's algorithm
   - `Search()`: Searches for patterns in the tree
   - `GetAllSuffixes()`: Returns all suffixes from the tree
   - `PrintTree()`: Visualizes the tree structure

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the text
- **Space Complexity**: O(n) for storing the suffix tree

## Usage Example

The implementation demonstrates building a suffix tree for "banana$" and provides functionality to search for patterns and retrieve all suffixes. The tree structure can be visualized through the print method.

This implementation provides a solid foundation for suffix tree operations and can be extended for more complex string processing tasks.

