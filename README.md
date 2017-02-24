# Elm project
I build this project as a learning project to learn Elm.
I am more than happy for any reviews or ideas where and how to improve.

I have no clue why the buttons look weird in FF but then CSS is still witchcraft for me :D

## What is it doing?
magic stuff :D

Apart from this it solves a problem I have, beeing a Capoeira teacher.
You have members and those members should pay an amount of money per month. But then there were holidays. Or someone could not attend for a month because his left big toe was itching pretty badly or his dog ate his training clothes or whatever. So they say: "I won't pay for august and in november I will only pay half the price (thats how it is. Deal with it)."
I think most of it is pretty self-explanatory but here are some Details:

    In order to get my money right,
    as a teacher,
    I want to handle the money I get as a monthly payment from attendees of my class.

    When I enter a new member
    Then a new member should be created without any payments or added months

    Given I have an active member "Sue" and an inactive member "John"
    When I add "December 2016" with an amount of 8€ to active members
    Then it will only be added to "Sue" because "John" was inactive

    Given I have an active members "Roman" and "Lena"
    And "December 2016" is already added to "Roman" with an amount of 8€
    When I add "December 2016"  with an amount of 10€ to active members
    Then it will only be added to "Lena"
    And "Roman" will still have "December 2016" with an amount of 10€


## What is it based on?
* Elm (obviously)
* Firebase for the backend
* some webpack/babel for transpilation of ES6 and the dev-server / hot-reloading
* [Bulma](http://bulma.io/) for the CSS/Flexbox magic
* https://github.com/moarwick/elm-webpack-starter


## Getting started
In order to try it yourself you need to create an own Firebase Database and change https://github.com/rommsen/elm-bookkeeping/blob/master/src/static/appfb.js#L4 accordingly

### Installation
You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

If you have yarn installed (you should) you can just run

    yarn run installation


**Otherwise do the following:**

Install JS dependencies with:

    [yarn|npm] install

Install Elm dependencies with:

    elm package install


### Running
Start webpack-dev-server (recompiles when files change)

    [yarn|npm] run start   

Then view it:

    localhost:8080
