using GuiaTuristica.API.Models;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore;

namespace GuiaTuristica.API.Data
{
    public class ApplicationDbContext : IdentityDbContext  // 2.Hereamos de IdentityDbContext el cual se encuentra en el paquete Nuget Microsoft.AspNetCore.Identity.EntityFrameworkCore;
    {
        public ApplicationDbContext(DbContextOptions options): base(options)
        {
            
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
        }

        public DbSet<Lugar> Lugares { get; set; }
    }
}
