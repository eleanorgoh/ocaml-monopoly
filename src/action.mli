
(** [t] is the abstract type of values representing an Action. 

    [t] is the outcome of a player action and is one of the following:
    (i) Jail if the player rolls three sets of doubles and goes to jail
    (ii) Steps representing the total number of steps a player takes
    if the player doesn't go to jail.
    (iii) Draw_Chance if the player should draw from the deck of Chance cards.
    (iv) Draw_Community if the player should draw from the deck of Community 
    Chest cards.
*)

type t = 
  | Jail
  | Step of int 
  | Draw_Chance 
  | Draw_Community

exception TransactionError of string
exception PlayerBankrupt of string

(** [roll_dice] is the outcome of a player's roll in one turn. *)
val roll_dice : t

(** [draw_community] is the action of drawing from the 
    Community Chest deck. *)
val draw_community : t

(** [roll_dice] is the action of drawing from the 
    Chance deck.*)
val draw_chance : t

(** [buy_property property player] is the new Player with their bought
    property. Raises TransactionError if the Player already owns the property
    or does not have enough money. *)
val buy_property :  Property.t -> Player.t -> Player.t

(** [sell_property player property] is the new Player after selling 
    [property]. The Player receives half of the original price of the 
    property back as money. Raises TransactionError if the Player doesn't own 
    this property. *)
val sell_property : Property.t -> Player.t -> Player.t

(** [buy_building property play] is the new Player with their bought
    building, e.g. house, hotel. Raises TransactionError if the Player doesn't 
    own this property, the Player does not have enough money, or the Player 
    cannot build another building on this property. *)
val buy_building : Property.t -> Player.t -> unit

(** [collect_rent collector payer property] subtracts the rent amount of 
    [property] from [payer]'s money and adds it to [collector]'s money. 
    Raises PlayerBankrupt if [payer] does not have enough money to pay 
    rent. *)
val collect_rent : Player.t -> Player.t -> Property.t -> unit
