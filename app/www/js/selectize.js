// const issuesInput = document.getElementById("#issue_type")
// const selectOptions = document.querySelectorAll("#issue_type div:")
// document.addEventListener("DOMContentLoaded", function (e) {
//   const selectInput = document.getElementById("issue_type")
//   console.log(selectInput)
//   const options = selectInput.getElementsByClassName("option")
//   console.log(options)
//   Object.keys(options).forEach(option => {
//     option[+option].addEventListener('click', (e) => {
//       constselectInput.getElementsByClassName('selected-custom')

//     })
//   })
// })
let options

const changeSelectedClass = (e) => {
  e.preventDefault()
  const selected = document.getElementsByClassName("selected")
  console.log("event fired")
}

const input = document.getElementById("issue_type")

input.addEventListener("click", (e) => {
  options = document.querySelectorAll(`.option`)
  console.log(options)

  options.forEach((option) => {
    option.addEventListener("click", (e) => changeSelectedClass(e))
  })
})
