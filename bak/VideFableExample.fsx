#r "nuget: Vide.Fable"

open System
open Vide
open type Vide.Html

type TodoItem = { name: string; mutable isDone: bool }
type TodoList = { items: TodoItem list }
    
let view = vide {
    let! todoList = Vide.ofMutable { items = [] }
        
    h1.class'("title") { "TODO List" }
    div {
        let! itemName = Vide.ofMutable ""
    
        p {
            let addItem () =
                let newItem = { name = itemName.Value; isDone = false }
                do
                    todoList.Value <- { todoList.Value with items = newItem :: todoList.Value.items }
                    itemName.Reset()
                
            input.bind(itemName)
            button
                .disabled(String.IsNullOrWhiteSpace(itemName.Value))
                .onclick(fun _ -> addItem()) { "Add Item" }
        }
    }
    div {
        for item in todoList.Value.items do
            div.class'("flex-row") {
                p { item.name }
                input.bind(item.isDone, fun value -> item.isDone <- value)
            }
    }
}
