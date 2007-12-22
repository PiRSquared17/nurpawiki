
// Register jscalendar selectors for cal_trigger named buttons.  Each
// cal_trigger named button has an id of form 'button_<n>' where <n>
// is the todo's DB id.  For each such element, there should exist a
// 'calendar_<n>' element that contains the date to be edited.
function nwRegisterCalendar()
{
    cals = document.getElementsByTagName("button");

    re = new RegExp("^cal_button_([0-9]+)$");

    for (var i = 0; i < cals.length; i++)
    {
        var m = re.exec(cals[i].id);
        
        if (m != null)
        {
            var todo_id = m[1];

            Calendar.setup(
                {
                  inputField  : "calendar_"+todo_id,
                  ifFormat    : "%Y-%m-%d",
                  button      : m[0]
                }
            );
        }

    }

}

addLoadEvent(nwRegisterCalendar);
