module [getNextRow, GalleryItem, LayoutItem, LayoutRow]

GalleryItem : {
}

LayoutItem : {
}

LayoutRow : {
    items : List LayoutItem,
    galleryWidth : Int U32,
    targetRowHeight : Int U32,
    currentRowItems : List GalleryItem,
    width : Int U32,
    removedItems : List GalleryItem,
    headings : List Str,
}

getNextRow : List GalleryItem, Int U32, Int U32, List GalleryItem, Int U32, List GalleryItem, List Str -> { row : LayoutRow, removedItems : List GalleryItem, remainingItems : List GalleryItem }
getNextRow = \items, galleryWidth, targetRowHeight, currentRowItems, width, removedItems, headings ->
    if List.len items == 0 then
        {
            row: {
                items: [],
                galleryWidth,
                targetRowHeight,
                currentRowItems,
                width,
                removedItems,
                headings,
            },
            removedItems: [],
            remainingItems: [],
        }
    else
        {
            row: {
                items: [],
                galleryWidth,
                targetRowHeight,
                currentRowItems,
                width,
                removedItems,
                headings,
            },
            removedItems: [],
            remainingItems: [],
        }

# Empty gallery returns empty layout.
expect
    getNextRow [] 10 20 [] 0 [] []
    == {
        row: {
            items: [],
            galleryWidth: 10,
            targetRowHeight: 20,
            currentRowItems: [],
            width: 0,
            removedItems: [],
            headings: [],
        },
        removedItems: [],
        remainingItems: [],
    }

# No items left returns current row.
expect
    currentRowItems = [{}, {}, {}]
    removedItems = [{}, {}]
    headings = ["a", "b"]
    out = getNextRow [] 10 21 currentRowItems 12 removedItems headings
    out
    == {
        row: LayoutRow {
            items: currentRowItems,
            offsetY: 0,
            height: 21,
            currentRowItems: [],
            width: 12,
            headings,
        },
        removedItems,
        remainingItems: [],
    }
