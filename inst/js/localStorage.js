const getLocalStorage = (key) => {
  if (typeof key === "undefined") {
    return Object.keys(localStorage)
      .reduce((acc, i) => {
        return {...acc, [i]: JSON.parse(localStorage.getItem(i))}
      }, {})
  } else {
    return JSON.parse(localStorage.getItem(key))
  }
}

const shinyReportLocalStorage = () => {
  if (Shiny.hasOwnProperty('setInputValue')) {
    Shiny.setInputValue('_localStorage', getLocalStorage())
  } else {
    setTimeout(shinyReportLocalStorage, 50);
  }
}

const setLocalStorage = ({key, value, overwrite = false}) => {
  if (typeof key === "undefined" || typeof value === "undefined") return;
  if (!Object.keys(localStorage).includes(key) || overwrite) {
    localStorage.setItem(key, JSON.stringify(value))
  }
  shinyReportLocalStorage()
}

const removeLocalStorage = (key) => {
  if (typeof key !== "undefined") {
    localStorage.removeItem(key)
  }
  shinyReportLocalStorage()
}

const clearLocalStorage = (clear) => {
  if (clear) localStorage.clear()
  shinyReportLocalStorage()
}

document.addEventListener('DOMContentLoaded', () => {
  shinyReportLocalStorage()

  Shiny.addCustomMessageHandler('setLocalStorage', setLocalStorage)
  Shiny.addCustomMessageHandler('removeLocalStorage', removeLocalStorage)
  Shiny.addCustomMessageHandler('clearLocalStorage', clearLocalStorage)
})
