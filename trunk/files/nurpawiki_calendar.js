
var cal1 = new CalendarPopup();

function selectDate()
{
    cal1.select(this, "act_date", "yyyy-MM-dd");
    return false;
}

function nwRegisterCalendar()
{
    elem = document.getElementById("activation_date");

    if(!elem)
    {
        alert("JS error: couldn't find activation_date");
    }
    elem.onclick = selectDate;
}

addLoadEvent(nwRegisterCalendar);
