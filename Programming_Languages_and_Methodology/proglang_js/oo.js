// constructor
function Person(name, age, job) {
    this.name = name;
    this.age = age;
    this.job = job;
    this.friends = ["Shelby", "Court"];

}
// prototype
Person.prototype = {
    constructor: Person,
    sayName: function() {
        return this.name;
    }
}
// instantiate
var person1 = new Person("Nicholas", 29, "Software Engineer");
var person2 = new Person("Greg", 27, "Doctor");

person1.friends.push("Van");
console.log(person1.friends);                     //output "Shelby,Count,Van"
console.log(person2.friends);                     //output "Shelby,Count"
console.log(person1.friends === person2.friends);        //output false
console.log(person1.sayName === person2.sayName);        //output true


