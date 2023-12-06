using GuiaTuristica.API.Data;
using GuiaTuristica.API.Models;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;

namespace GuiaTuristica.API.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class LugaresController : ControllerBase
    {
        private readonly ApplicationDbContext context;

        public LugaresController(ApplicationDbContext context)
        {
            this.context = context;
        }

        [Authorize(AuthenticationSchemes = JwtBearerDefaults.AuthenticationScheme)]
        [HttpGet]
        public async Task<ActionResult<List<Lugar>>> GetLugares()
        {
            return await context.Lugares.ToListAsync();
        }

        [HttpPost]
        public async Task<ActionResult> PostLugar(Lugar lugar)
        {
            context.Add(lugar);
            await context.SaveChangesAsync();
            return Ok();
        }

        [HttpGet("lugar/{id}")]
        public async Task<ActionResult<Lugar>> GetLugar(int id)
        {
            var lugar = await context.Lugares.FirstOrDefaultAsync(x => x.Id == id);
            if (lugar == null)
            {
                return NotFound();
            }
            return lugar;
        }   
    }
}
